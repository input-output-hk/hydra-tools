{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib.Bridge.GitHubToHydra
  ( hydraClientEnv,
    app,
    singleEndpoint,
    pushHook,
    escapeHydraName,
    parseMergeQueueRef,
    issueCommentHook,
    pullRequestHook,
    checkSuiteHook,
    checkRunHook,
    hydraClient,
    handleCmd,
    on404,
    splitRepo,
    reAuthenticate,
  )
where

import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Char (isNumber)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple (Connection)
import GitHub.Data.Webhooks.Events (CheckRunEvent (..), CheckRunEventAction (..), CheckSuiteEvent (..), CheckSuiteEventAction (..), IssueCommentEvent (..), PullRequestEvent (..), PullRequestEventAction (..), PushEvent (..))
import GitHub.Data.Webhooks.Payload (HookCheckRun (..), HookCheckSuite (..), HookChecksPullRequest (..), HookChecksPullRequestTarget (..), HookIssueComment (..), HookPullRequest (..), HookRepository (..), HookUser (..), PullRequestTarget (..))
import Lib.GitHub.WebHookServer (GitHubKey, SingleHookEndpointAPI)
import Lib.Hydra.Client (HydraJobset (..), defHydraFlakeJobset, getJobset, HydraClientEnv (..), HydraProject (..), push, mkJobset, mkProject, defHydraProject, rmJobset, restartBuild, login, HydraLogin (..), isAuthError)
import Lib.Hydra.DB (Command (..), writeCommand, readCommand)
import Servant (Application, Context (..), Handler, Server, (:<|>) (..))
import Servant.Client (ClientEnv (..), runClientM, ClientM, ClientError (..), ResponseF (..), mkClientEnv, parseBaseUrl, BaseUrl (..), Scheme (..), showBaseUrl)
import Servant.GitHub.Webhook (RepoWebhookEvent)
import Servant.Server (serveWithContext)
import System.Exit (die)
import Text.Read (readMaybe)
import Network.HTTP.Types (Status(..))
import Control.Monad.Except (MonadError(..))
import Network.URI (parseURI)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager)
import Control.Concurrent.STM (newTVarIO)

app :: ClientEnv -> Connection -> GitHubKey -> Application
app env conn key =
  serveWithContext
    (Proxy :: Proxy SingleHookEndpointAPI)
    (key :. EmptyContext)
    (singleEndpoint env conn)

singleEndpoint :: ClientEnv -> Connection -> Server SingleHookEndpointAPI
singleEndpoint env conn =
  (pushHook conn)
    :<|> issueCommentHook
    :<|> (pullRequestHook conn)
    :<|> (checkSuiteHook env conn)
    :<|> (checkRunHook conn)

pushHook :: Connection -> RepoWebhookEvent -> ((), PushEvent) -> Handler ()
pushHook conn _ (_, PushEvent {evPushRef = ref, evPushHeadSha = Just headSha, evPushRepository = HookRepository {whRepoFullName = repoName}})
  | "refs/heads/gh-readonly-queue/" `Text.isPrefixOf` ref,
    Just (targetBranch, pullReqNumber) <- parseMergeQueueRef ref,
    "0000000000000000000000000000000000000000" == headSha =
      liftIO $ do
        let projName = escapeHydraName repoName
            jobsetName = "merge-queue-" <> Text.pack (show pullReqNumber)

        let jobset =
              defHydraFlakeJobset
                { hjName = "merge-queue-" <> Text.pack (show pullReqNumber),
                  hjDescription = "Merge Queue: PR" <> Text.pack (show pullReqNumber) <> " -> " <> targetBranch,
                  hjFlake = "github:" <> repoName <> "/" <> headSha,
                  -- setting visiblity seems to have no effect...
                  hjVisible = False,
                  -- ... so we just disable it.
                  hjEnabled = 0
                }
        -- We Update the Jobset instead of Delete, so that past build results will
        -- still be available.  This should update the sha to 000000, and as such
        -- allow us to find them and delete them later.
        do
          putStrLn $ "Adding Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
          writeCommand conn (UpdateJobset repoName projName jobsetName jobset)
  | "refs/heads/gh-readonly-queue/" `Text.isPrefixOf` ref,
    Just (targetBranch, pullReqNumber) <- parseMergeQueueRef ref =
      liftIO $ do
        let jobset =
              defHydraFlakeJobset
                { hjName = "merge-queue-" <> Text.pack (show pullReqNumber),
                  hjDescription = "Merge Queue: PR" <> Text.pack (show pullReqNumber) <> " -> " <> targetBranch,
                  hjFlake = "github:" <> repoName <> "/" <> headSha
                }

            projName = escapeHydraName repoName
            jobsetName = "merge-queue-" <> Text.pack (show pullReqNumber)

        do
          putStrLn $ "Adding Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
          writeCommand conn (CreateOrUpdateJobset repoName projName jobsetName jobset)
  | ref `elem` ["refs/heads/" <> x | x <- ["main", "master", "develop"]]
      || any (`Text.isPrefixOf` ref) ["refs/" <> x <> "/" | x <- ["tags", "heads/release", "heads/ci"]] =
      liftIO $ do
        let projName = escapeHydraName repoName
            refName = maybe (Text.drop (Text.length "refs/tags/") ref) id $ Text.stripPrefix "refs/heads/" ref
            jobsetName = escapeHydraName refName
            jobset =
              defHydraFlakeJobset
                { hjName = jobsetName,
                  hjDescription = refName <> " " <> if "refs/heads/" `Text.isPrefixOf` ref then "branch" else "tag",
                  hjFlake = "github:" <> repoName <> "/" <> headSha
                }

        do
          putStrLn $ "Adding Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
          writeCommand conn (CreateOrUpdateJobset repoName projName jobsetName jobset)
pushHook _conn _ (_, ev) = liftIO $ do
  putStrLn $ (show . whUserLogin . fromJust . evPushSender) ev ++ " pushed a commit causing HEAD SHA to become:"
  print $ (fromJust . evPushHeadSha) ev

parseMergeQueueRef :: Text -> Maybe (Text, Int)
parseMergeQueueRef ref = do
  suffix <- Text.stripPrefix "refs/heads/gh-readonly-queue/" ref
  let (branchName, prPart) = Text.breakOn "/pr-" suffix
  rest <- Text.stripPrefix "/pr-" prPart
  let prNumberText = Text.takeWhile isNumber rest
  prNumber <- readMaybe (Text.unpack prNumberText) :: Maybe Int

  return (branchName, prNumber)

escapeHydraName :: Text -> Text
escapeHydraName = Text.replace "/" "-" . Text.replace "." "-"

issueCommentHook :: RepoWebhookEvent -> ((), IssueCommentEvent) -> Handler ()
issueCommentHook _ (_, ev) = liftIO $ do
  putStrLn "An issue comment was posted:"
  print $ (whIssueCommentBody . evIssueCommentPayload) ev

pullRequestHook :: Connection -> RepoWebhookEvent -> ((), PullRequestEvent) -> Handler ()
pullRequestHook
  conn
  _
  ( _,
    ev@PullRequestEvent
      { evPullReqAction = action,
        evPullReqPayload = HookPullRequest {whPullReqIsDraft = isDraft},
        evPullReqRepo = HookRepository {whRepoFullName = repoName}
      }
    )
    | action
        `elem` [ PullRequestOpenedAction,
                 PullRequestReopenedAction,
                 (PullRequestActionOther "synchronize")
               ]
        && ( repoName
               `notElem` [ "IntersectMBO/ouroboros-network",
                           "IntersectMBO/cardano-cli",
                           "IntersectMBO/cardano-api"
                         ]
               || not (maybe False id isDraft)
           ) =
        liftIO $ do
          -- we now want to send a request to
          -- \$hydraApiUrl
          -- with
          -- POST /jobset/:project-id/:jobset-id
          -- { name: string
          -- , description: string || null
          -- , nixexprinput: string || null
          -- , nixexprpath: string || null
          -- , errormsg: string || null
          -- , errortime: string || null
          -- , lastcheckedtime: integer || null
          -- , triggertime: integer || null
          -- , enabled: integer (0: disabled, 1: enabled, 2: one-shot, 3: one-at-a-time)
          -- , enableemail: boolean
          -- , enable_dynamic_run_command: boolean
          -- , visible: boolean
          -- , emailoverride: string
          -- , keepnr: integer
          -- , checkinterval: integer
          -- , schedulingshares: integer
          -- , fetcherrormsg: string || null
          -- , startime: integer || null
          -- , type: integer
          -- , flake: flake-uri || null
          -- , inputs: ...
          -- } : Jobset
          --
          -- enabled = 1;
          -- hidden = false;
          -- keepnr = 5;
          -- schedulingshares = 42;
          -- checkinterval = 60;
          -- enableemail = false;
          -- emailoverride = "";
          --
          --  type: 1
          --
          --  name: pullrequest-{n}
          --  description: PR {n}: {pr title}
          --  flake = "github:${info.head.repo.owner.login}/${info.head.repo.name}/${info.head.ref}";
          --
          let jobset =
                defHydraFlakeJobset
                  { hjName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev)),
                    hjDescription = "PR " <> Text.pack (show (evPullReqNumber ev)) <> ": " <> whPullReqTitle (evPullReqPayload ev),
                    hjFlake =
                      "github:"
                        <> repoName
                        <> "/"
                        <> whPullReqTargetSha (whPullReqHead (evPullReqPayload ev))
                  }

              projName = escapeHydraName repoName
              jobsetName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev))

          liftIO $ do
            putStrLn $ "Adding Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
            writeCommand conn (CreateOrUpdateJobset repoName projName jobsetName jobset)
pullRequestHook conn _ (_, ev@PullRequestEvent {evPullReqAction = PullRequestClosedAction}) = liftIO $ do
  let repoName = whRepoFullName (evPullReqRepo ev)
      projName = escapeHydraName repoName
      jobsetName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev))

  let jobset =
        defHydraFlakeJobset
          { hjName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev)),
            hjDescription = "PR " <> Text.pack (show (evPullReqNumber ev)) <> ": " <> whPullReqTitle (evPullReqPayload ev),
            hjFlake =
              "github:"
                <> repoName
                <> "/"
                <> whPullReqTargetSha (whPullReqHead (evPullReqPayload ev)),
            -- setting visiblity seems to have no effect...
            hjVisible = False,
            -- ... so we just disable it.
            hjEnabled = 0
          }

  -- We Update the Jobset instead of Delete, so that past build results will
  -- still be available.  This should update the sha to 000000, and as such
  -- allow us to find them and delete them later.
  liftIO $ do
    putStrLn $ "Adding Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
    writeCommand conn (UpdateJobset repoName projName jobsetName jobset)
pullRequestHook _ _ (_, ev) =
  liftIO (putStrLn $ "Unhandled pullRequestEvent with action: " ++ show (evPullReqAction ev))

checkSuiteHook :: ClientEnv -> Connection -> RepoWebhookEvent -> ((), CheckSuiteEvent) -> Handler ()
checkSuiteHook env conn _ (_, ev@CheckSuiteEvent {evCheckSuiteAction = CheckSuiteEventActionRerequested}) = liftIO $ do
  let prs = whCheckSuitePullRequests $ evCheckSuiteCheckSuite ev
      repoName = whRepoFullName $ evCheckSuiteRepository ev
      projName = escapeHydraName repoName

  forM_ prs $ \pr -> do
    let jobsetName = "pullrequest-" <> Text.pack (show $ whChecksPullRequestNumber pr)

    jobset <-
      (flip runClientM env $ getJobset projName jobsetName)
        >>= either (die . show) return
        >>= \response -> case Aeson.fromJSON response of
          Aeson.Error e -> die $ show e
          Aeson.Success v -> return v

    putStrLn $ "Adding Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
    writeCommand conn $
      UpdateJobset repoName projName jobsetName $
        jobset
          { hjFlake = "github:" <> repoName <> "/" <> whChecksPullRequestTargetSha (whChecksPullRequestHead pr)
          }
checkSuiteHook _ _ _ (_, ev) = liftIO . putStrLn $ "Unhandled checkSuiteEvent with action: " ++ show (evCheckSuiteAction ev) ++ "; payload: " ++ show ev

checkRunHook :: Connection -> RepoWebhookEvent -> ((), CheckRunEvent) -> Handler ()
checkRunHook conn _ (_, ev@CheckRunEvent {evCheckRunAction = CheckRunEventActionRerequested}) = liftIO $ do
  let checkRun = evCheckRunCheckRun ev
      checkRunName = whCheckRunName checkRun
      repoName = whRepoFullName $ evCheckRunRepository ev
      projName = escapeHydraName repoName
      prs = whCheckRunPullRequests checkRun

  if "ci/eval" `Text.isPrefixOf` checkRunName
    then forM_ prs $ \pr -> do
      let jobsetName = "pullrequest-" <> Text.pack (show $ whChecksPullRequestNumber pr)

      putStrLn $ "Adding Eval " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
      writeCommand conn $ EvaluateJobset projName jobsetName True
    else do
      let externalId = read . Text.unpack $ whCheckRunExternalId checkRun
      putStrLn $ "Adding Restart " ++ Text.unpack checkRunName ++ " #" ++ show externalId ++ " to the queue."
      writeCommand conn $ RestartBuild externalId
checkRunHook _ _ (_, ev) =
  liftIO . putStrLn $
    "Unhandled checkRunEvent with action: "
      ++ show (evCheckRunAction ev)
      ++ "; payload: ["
      ++ show (whCheckRunHeadSha (evCheckRunCheckRun ev))
      ++ "] "
      ++ show (whCheckRunName (evCheckRunCheckRun ev))
      ++ ": "
      ++ show (whCheckRunStatus (evCheckRunCheckRun ev))

hydraClientEnv :: Text -> Text -> Text -> IO HydraClientEnv
hydraClientEnv host user pass = do
  mgr <- newManager tlsManagerSettings
  jar <- newTVarIO mempty

  -- Parse host string
  let hostStr = Text.unpack host
  hydraUrl <-
    case parseURI hostStr of
      -- It's a valid URI, use servant's parser
      Just _ -> parseBaseUrl hostStr
      -- Otherwise, we'll just assume it's a bare host
      _ -> pure (BaseUrl Https hostStr 443 "")

  let env =
        (mkClientEnv mgr hydraUrl)
          { cookieJar = Just jar
          }
      -- The base url will be passed around in the origin header
      host' = Text.pack (showBaseUrl hydraUrl)

  -- login first
  runClientM (login (Just host') (HydraLogin user pass)) env >>= \case
    Left e -> die (show e)
    Right _ -> pure ()

  return $ HydraClientEnv host user pass env

hydraClient :: HydraClientEnv -> Connection -> IO ()
hydraClient henv@(HydraClientEnv host _ _ env) conn =
  -- loop forever, working down the hydra commands
  forever $
    readCommand conn >>= \cmd -> do
      result <- runClientM (handleCmd host cmd) env
      case result of
        Left e | isAuthError e -> do
          putStrLn "Authentication error detected, re-authenticating..."
          reAuthenticate henv >>= \case
            Left authErr -> putStrLn authErr
            Right () -> do
              -- Retry the command after successful re-authentication
              runClientM (handleCmd host cmd) env >>= \case
                Left e' -> print e'
                Right _ -> pure ()
        Left e -> print e
        Right _ -> pure ()

handleCmd :: Text -> Command -> ClientM ()
handleCmd host (CreateOrUpdateJobset repoName projName jobsetName jobset) = do
  liftIO (putStrLn $ "Processing Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue.")
  void $
    mkJobset projName jobsetName jobset `on404` do
      let proj = snd $ splitRepo repoName
      void $
        mkProject
          projName
          ( defHydraProject
              { hpName = projName,
                hpDisplayname = proj,
                hpHomepage = "https://github.com/" <> repoName
              }
          )
      mkJobset projName jobsetName jobset

  liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " triggering push...")
  void $ push (Just host) (Just (projName <> ":" <> jobsetName)) Nothing
  return ()
handleCmd host (UpdateJobset repoName projName jobsetName jobset) = do
  liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue.")
  -- ensure we try to get this first, ...
  void $ getJobset projName jobsetName
  -- if get fails, no point in making one.
  void $
    mkJobset projName jobsetName jobset `on404` do
      let proj = snd $ splitRepo repoName
      void $
        mkProject
          projName
          ( defHydraProject
              { hpName = projName,
                hpDisplayname = proj,
                hpHomepage = "https://github.com/" <> repoName
              }
          )
      mkJobset projName jobsetName jobset

  -- or triggering an eval
  liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " triggering push...")
  void $ push (Just host) (Just (projName <> ":" <> jobsetName)) Nothing
  return ()
handleCmd _ (DeleteJobset projName jobsetName) = do
  void $ rmJobset projName jobsetName
  return ()
handleCmd host (EvaluateJobset projName jobsetName force) = do
  liftIO (putStrLn $ "Processing Eval " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue. Triggering push...")
  void $ push (Just host) (Just (projName <> ":" <> jobsetName)) (Just force)
  return ()
handleCmd _ (RestartBuild bid) = do
  liftIO (putStrLn $ "Processing Restart " ++ show bid ++ " from the queue. Triggering restart...")
  void $ restartBuild $ bid
  return ()

-- combinator for handing 404 (not found)
on404 :: ClientM a -> ClientM a -> ClientM a
on404 a b = a `catchError` handle
  where
    handle (FailureResponse _ (Response {responseStatusCode = Status {statusCode = 404}})) = b
    handle e = throwError e

splitRepo :: Text -> (Text, Text)
splitRepo repo =
  case Text.splitOn "/" repo of
    (org : proj : _) -> (org, proj)
    _ -> error $ "Lib.splitrepo on " ++ Text.unpack repo

-- Re-authenticate with Hydra when session expires
reAuthenticate :: HydraClientEnv -> IO (Either String ())
reAuthenticate (HydraClientEnv host user pass env) = do
  result <- runClientM (login (Just host) (HydraLogin user pass)) env
  case result of
    Left e -> return $ Left ("Re-authentication failed: " ++ show e)
    Right _ -> return $ Right ()
