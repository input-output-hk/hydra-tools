{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Lib.Bridge.HydraToGitHub
  ( HydraToGitHubEnv (..),
    HydraToGitHubT (..),
    runHydraToGitHubT,
    fetchGitHubTokens,
    statusHandler,
    toHydraNotification,
    handleHydraNotification,
    notificationWatcher,
    statusHandlers,
    parseGitHubFlakeURI,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async as Async
import Control.Exception
  ( SomeException,
    catch,
    displayException,
    fromException,
    handleJust,
    throw,
    toException,
    try,
  )
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Aeson hiding (Error, Success)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Duration (oneSecond)
import Data.Foldable (foldr')
import Data.Functor ((<&>))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List (find, intercalate, singleton)
import Data.Maybe (fromMaybe, isNothing)
import Data.String (fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Time.Clock (addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
  ( getPOSIXTime,
    posixSecondsToUTCTime,
  )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Debug.Trace (traceShowId)
import GitHub.REST
  ( GHEndpoint (..),
    GitHubSettings (..),
    KeyValue ((:=)),
    StdMethod (POST),
    queryGitHub,
  )
import Lib (binarySearch)
import Lib.Data.Duration (humanReadableDuration)
import Lib.Data.List (takeEnd)
import Lib.Data.Text (indentLine)
import Lib.GitHub (CheckRun, CheckRunConclusion, TokenLease)
import Lib.GitHub qualified as GitHub
import Lib.Hydra (BuildStatus)
import Lib.Hydra qualified as Hydra
import Lib.Hydra.DB qualified as DB
import Network.HTTP.Client qualified as HTTP
import Text.Regex.TDFA ((=~))

-- Text utils
tshow :: (Show a) => a -> Text
tshow = cs . show

data HydraToGitHubEnv = HydraToGitHubEnv
  { htgEnvHydraHost :: String,
    htgEnvHydraStateDir :: String,
    htgEnvGhAppId :: Int,
    htgEnvGhAppKeyFile :: FilePath,
    htgEnvGhEndpointUrl :: Text,
    htgEnvGhUserAgent :: ByteString,
    htgEnvGhAppInstallIds :: [(Text, Int)],
    htgEnvGhTokens :: IORef [(String, TokenLease)]
  }
  deriving (Eq)

newtype HydraToGitHubT m a = HydraToGitHubT
  { unHydraToGitHubT :: ReaderT HydraToGitHubEnv m a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader HydraToGitHubEnv
    )

runHydraToGitHubT :: HydraToGitHubEnv -> HydraToGitHubT m a -> m a
runHydraToGitHubT env (HydraToGitHubT action) = runReaderT action env

fetchGitHubTokens ::
  Int ->
  FilePath ->
  Text ->
  ByteString ->
  [(Text, Int)] ->
  IO [(String, TokenLease)]
fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent ghAppInstallIds = do
  -- Fetch installations
  putStrLn "Fetching GitHub App installations..."
  ghAppInstalls <- GitHub.fetchInstallations ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent
  putStrLn $ "Found " <> show (length ghAppInstalls) <> " installations"
  forM_ ghAppInstalls $ \(owner, installId) -> do
    Text.putStrLn $ "\t- " <> owner <> " (" <> Text.pack (show installId) <> ")"

  -- Filter out installations not configured
  appInstalls <- flip filterM ghAppInstalls $ \inst@(owner, installId) -> do
    let found = inst `elem` ghAppInstallIds
    unless found $
      Text.putStrLn $
        "Warning: No configured GitHub App Installation ID: "
          <> owner
          <> " ("
          <> Text.show installId
          <> ")"

    pure found

  -- Fetch app installation tokens
  forM appInstalls $ \(owner, installId) -> do
    lease <- GitHub.fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent installId
    Text.putStrLn $ "Fetched new GitHub App installation token valid for " <> owner <> " until " <> Text.pack (show lease.expiry)
    return (Text.unpack owner, lease)

notificationWatcher :: Connection -> HydraToGitHubT IO ()
notificationWatcher conn = do
  host <- asks htgEnvHydraHost
  stateDir <- asks htgEnvHydraStateDir

  liftIO $ do
    _ <- execute_ conn "LISTEN eval_started" -- (opaque id, jobset id)
    _ <- execute_ conn "LISTEN eval_added" -- (opaque id, jobset id, eval record id)
    _ <- execute_ conn "LISTEN eval_cached" -- (opaque id, jobset id, prev identical eval id)
    _ <- execute_ conn "LISTEN eval_failed" -- (opaque id, jobset id)
    _ <- execute_ conn "LISTEN build_queued" -- (build id)
    _ <- execute_ conn "LISTEN cached_build_queued" -- (eval id, build id)
    _ <- execute_ conn "LISTEN build_started" -- (build id)
    _ <- execute_ conn "LISTEN build_finished" -- (build id, dependent build ids...)
    _ <- execute_ conn "LISTEN cached_build_finished" -- (eval id, build id)
    forever $ do
      putStrLn "Waiting for notification..."
      note <- toHydraNotification . traceShowId <$> getNotification conn
      statuses <- handleHydraNotification conn (cs host) stateDir note
      forM_ statuses $
        ( \(GitHub.CheckRun owner repo payload) -> do
            liftIO $ Text.putStrLn $ "QUEUEING [" <> owner <> "/" <> repo <> "/" <> payload.headSha <> "] " <> payload.name <> ":" <> Text.pack (show payload.status)
            [Only _id'] <-
              query
                conn
                "with status_upsert as (insert into github_status (owner, repo, headSha, name) values (?, ?, ?, ?) on conflict (owner, repo, headSha, name) do update set name = excluded.name returning id) insert into github_status_payload (status_id, payload) select (select id from status_upsert), ? returning id"
                (owner, repo, payload.headSha, payload.name, (toJSON payload)) ::
                IO [Only Int]
            execute_ conn "NOTIFY github_status"
        )

statusHandlers :: Connection -> HydraToGitHubT IO ()
statusHandlers conn = do
  ghEndpointUrl <- asks htgEnvGhEndpointUrl
  ghUserAgent <- asks htgEnvGhUserAgent
  env <- ask

  forever $ do
    let processStatuses = withTransaction conn $ do
          rows <-
            query_
              conn
              ( fromString $
                  unwords
                    [ "WITH AllStatus AS (",
                      "  SELECT s.id, MAX(p.id) AS mostRecentPaylodID, s.owner, s.repo, s.headSha, s.name",
                      "  FROM github_status s",
                      "  JOIN github_status_payload p ON s.id = p.status_id",
                      "  GROUP BY s.id, s.owner, s.repo, s.headSha, s.name",
                      ")",
                      "SELECT p.id, g.owner, g.repo, p.payload",
                      "FROM AllStatus g",
                      "JOIN github_status_payload p ON g.id = p.status_id",
                      "WHERE p.id = g.mostRecentPaylodID AND p.sent IS NULL AND p.tries < 5",
                      "ORDER BY",
                      "  CASE WHEN g.name = 'ci/eval' THEN 0 ELSE 1 END,", -- Prioritize 'ci/eval'
                      "  p.id ASC",
                      "LIMIT 1",
                      "FOR UPDATE SKIP LOCKED"
                      -- "SELECT p.id, g.owner, g.repo, p.payload"
                      --                              , "FROM github_status_payload p"
                      --                              , "JOIN github_status g ON g.id = p.status_id"
                      --                              , "WHERE p.sent IS NULL AND p.tries < 5"
                      --                              , "ORDER BY p.created ASC"
                      --                              , "FOR UPDATE OF p, g SKIP LOCKED"
                    ]
              )
          -- by sorting on p.created, we can assume that "newer" statuses for the same owner/repo/sha/name, are
          -- returned last. This is only applicable if we find multiple rows. If we find only a single row this
          -- is irrelevant. However for multiple rows, the last item will be the most recent status and we can just
          -- send that to GitHub, and skip all prior ones. They usually go through queued -> in_progress -> completed.
          -- If we already have the status for completed, we don't need to send queued, and in_progress. This would
          -- just eat two requests, which for many concurrent status-updates can lead to a lot of requests, and thus
          -- us running into rate-limits.
          case (reverse rows) of
            (id', owner, repo, payload) : _ -> do
              let payload' = case fromJSON payload of
                    Aeson.Success p -> p
                    Aeson.Error e -> error e

              -- putStrLn $ "Obtain GitHub token..."
              ghToken <- getValidGitHubTokenIO env
              -- putStrLn $ "GitHub Token: " <> show ghToken

              eres <- statusHandler ghEndpointUrl ghUserAgent ghToken (GitHub.CheckRun owner repo payload')
              case eres of
                Left ex
                  | Just (HTTP.HttpExceptionRequest _req (HTTP.StatusCodeException resp _)) <- fromException ex,
                    Just n <- read . BS.unpack <$> lookup "Retry-After" (HTTP.responseHeaders resp) -> do
                      putStrLn $ "Hit the rate-limit: Retrying in " <> show n <> " seconds..."
                      threadDelay (n * 1000000)
                      return ()
                Left ex
                  | Just (HTTP.HttpExceptionRequest _req (HTTP.StatusCodeException resp _)) <- fromException ex,
                    Just remaining <- read . BS.unpack <$> lookup "X-RateLimit-Remaining" (HTTP.responseHeaders resp),
                    remaining == (0 :: Int),
                    Just utc_epoch_offset <- read . BS.unpack <$> lookup "X-RateLimit-Reset" (HTTP.responseHeaders resp) -> do
                      current_utc_epoch <- round <$> getPOSIXTime
                      putStrLn $ "Hit the rate-limit: Retrying in " <> show (utc_epoch_offset - current_utc_epoch) <> " seconds..."
                      threadDelay ((utc_epoch_offset - current_utc_epoch) * 1000000)
                      return ()
                Left ex
                  | Just (HTTP.HttpExceptionRequest _req HTTP.ConnectionTimeout) <- fromException ex -> do
                      putStrLn "Connection timeout, retrying..."
                      return ()
                Left e -> do
                  Text.putStrLn $ "FAIL [" <> owner <> "/" <> repo <> "/" <> payload'.headSha <> "] " <> payload'.name <> ":" <> Text.pack (show payload'.status) <> ": " <> Text.pack (show e)
                  _ <- execute conn "UPDATE github_status_payload SET tries = tries + 1 WHERE id = ?" (Only id' :: Only Int)
                  return ()
                Right _res -> do
                  Text.putStrLn $ "SENT [" <> owner <> "/" <> repo <> "/" <> payload'.headSha <> "] " <> payload'.name <> ":" <> Text.pack (show payload'.status)
                  -- mark all statuses as sent; previous statueses are overridded anyway.
                  forM_ rows $ \(_id, o, r, p) -> do
                    case fromJSON p of
                      Aeson.Success (p' :: GitHub.CheckRunPayload) ->
                        Text.putStrLn $ "MARK [" <> o <> "/" <> r <> "/" <> p'.headSha <> "] " <> p'.name <> ":" <> Text.pack (show p'.status) <> " SENT"
                      Aeson.Error e -> error e
                  _ <- execute conn "UPDATE github_status_payload SET sent = NOW() WHERE id IN ?" (Only (In [id'' | (id'', _, _, _) <- rows] :: In [Int]))
                  -- BSL.putStrLn $ "<- " <> encode res
                  return ()
              return True
            _ -> return False
    _ <- liftIO $ execute_ conn "LISTEN github_status"
    let loop = do
          executed <- liftIO processStatuses
          unless executed $ void $ liftIO $ getNotification conn
          when executed loop
    loop

getValidGitHubTokenIO :: HydraToGitHubEnv -> IO [(String, TokenLease)]
getValidGitHubTokenIO HydraToGitHubEnv {..} = do
  -- Fetch tokens from in-memory state. If they are set to expire within 5 seconds,
  -- refresh them from GitHub.
  let buffer = 5 :: NominalDiffTime

  liftIO $ getValidToken buffer htgEnvGhTokens $ \owner -> do
    putStrLn $ "GitHub token expired or will expire within the next " <> show buffer <> ", fetching a new one..."

    -- Lookup the installation for the owner in the configured App Installation IDs.
    -- If found, fetch the token. If we don't know about it, we don't want to use it
    -- (eg, if a stranger found and installed our app).
    let ghAppInstallId = fmap snd . find ((owner ==) . Text.unpack . fst) $ htgEnvGhAppInstallIds
    case ghAppInstallId of
      Nothing -> do
        Text.putStrLn $ "Warning: No configured GitHub App Installation ID " <> Text.pack owner
        pure Nothing
      Just inst -> do
        res <-
          GitHub.fetchAppInstallationToken
            htgEnvGhEndpointUrl
            htgEnvGhAppId
            htgEnvGhAppKeyFile
            htgEnvGhUserAgent
            inst
        pure (Just res)

-- | Look up tokens from in-memory state. If they are set to expire within 'buffer', run
-- the 'fetch' action.
getValidToken ::
  NominalDiffTime ->
  IORef [(String, TokenLease)] ->
  (String -> IO (Maybe TokenLease)) ->
  IO [(String, TokenLease)]
getValidToken buffer lease fetch = do
  leases' <- readIORef lease
  now <- getCurrentTime
  leases'' <- forM leases' $ \lease'@(owner, tok) -> do
    case tok.expiry of
      Just expiry' | addUTCTime buffer now < expiry' -> return (owner, tok)
      -- If `fetch` doesn't return a lease, ignore it rather than remove it from the
      -- list. This is okay because unknown installations should have been removed at
      -- application startup anyways.
      _ -> maybe lease' (owner,) <$> fetch owner
  writeIORef lease leases''
  return leases''

toHydraNotification :: Notification -> Hydra.Notification
toHydraNotification Notification {notificationChannel = chan, notificationData = payload}
  | chan == "eval_started", [_, jid] <- words (cs payload) = Hydra.EvalStarted (read jid)
  | chan == "eval_added", [_, jid, eid] <- words (cs payload) = Hydra.EvalAdded (read jid) (read eid)
  | chan == "eval_cached", [_, jid, eid] <- words (cs payload) = Hydra.EvalCached (read jid) (read eid)
  | chan == "eval_failed", [_, jid] <- words (cs payload) = Hydra.EvalFailed (read jid)
  | chan == "build_queued", [bid] <- words (cs payload) = Hydra.BuildQueued (read bid)
  | chan == "cached_build_queued", [_, bid] <- words (cs payload) = Hydra.BuildQueued (read bid)
  | chan == "build_started", [bid] <- words (cs payload) = Hydra.BuildStarted (read bid)
  | chan == "build_finished", (bid : depBids) <- words (cs payload) = Hydra.BuildFinished (read bid) (map read depBids)
  | chan == "cached_build_finished", [_, bid] <- words (cs payload) = Hydra.BuildFinished (read bid) []
  | otherwise = error $ "Unhandled payload for chan: " ++ cs chan ++ ": " ++ cs payload

whenStatusOrJob :: Maybe GitHub.CheckRunConclusion -> Maybe Hydra.BuildStatus -> Text -> IO [GitHub.CheckRun] -> IO [GitHub.CheckRun]
whenStatusOrJob status prevStepStatus job action
  | or [name `Text.isPrefixOf` job || name `Text.isSuffixOf` job || ("." <> name <> ".") `Text.isInfixOf` job | name <- ["required", "nonrequired"]] = action
  | Just s <- status, s `elem` [GitHub.Failure, GitHub.Cancelled, GitHub.Stale, GitHub.TimedOut] = action
  | Just pss <- prevStepStatus, pss /= Hydra.Succeeded && maybe True (== GitHub.Success) status = action
  | otherwise = Text.putStrLn ("Ignoring job: " <> job) >> pure []

withGithubFlake :: Text -> (Text -> Text -> Text -> IO [GitHub.CheckRun]) -> IO [GitHub.CheckRun]
withGithubFlake flake action
  | Just (owner, repo, hash) <- parseGitHubFlakeURI flake = action owner repo hash
  | otherwise = Text.putStrLn ("Failed to parse flake: " <> flake) >> pure []

parseGitHubFlakeURI :: Text -> Maybe (Text, Text, Text)
parseGitHubFlakeURI uri
  | "github:" `Text.isPrefixOf` uri =
      case splitFlakeRef (Text.drop 7 uri) of
        -- TODO: hash == 40 is a _very_ poor approximation to ensure this is a sha
        Just (owner, repo, hash) | Text.length hash == 40 -> Just (owner, repo, hash)
        Just (owner, repo, hash)
          | (hash' : _) <- Text.splitOn "?" hash,
            Text.length hash' == 40 ->
              Just (owner, repo, hash')
        _ -> Nothing
  | otherwise = Nothing
  where
    splitFlakeRef t =
      case Text.splitOn "/" t of
        -- Query parameters can contain slashes that we don't want to split, so combine everything
        -- after repo
        (owner : repo : ts) -> Just (owner, repo, Text.concat ts)
        _ -> Nothing

handleHydraNotification :: Connection -> Text -> FilePath -> Hydra.Notification -> IO [GitHub.CheckRun]
handleHydraNotification conn host stateDir notification =
  handleJust catchJustPredicate (handler notification) $
    case notification of
      -- Evaluations
      (Hydra.EvalStarted jid) -> handleEvalStarted conn host jid
      (Hydra.EvalAdded jid eid) -> handleEvalDone conn host stateDir jid eid "Added"
      (Hydra.EvalCached jid eid) -> handleEvalDone conn host stateDir jid eid "Cached"
      (Hydra.EvalFailed jid) -> handleEvalFailed conn host jid
      -- Builds
      (Hydra.BuildQueued bid) -> handleBuildQueued conn host bid
      (Hydra.BuildStarted bid) -> handleBuildStarted conn host bid
      (Hydra.BuildFinished bid depBids) -> handleBuildFinished conn host stateDir bid depBids
  where
    catchJustPredicate ee
      | Just (_ :: Async.AsyncCancelled) <- fromException ee = Nothing
      | otherwise = Just ee

    handler :: Hydra.Notification -> SomeException -> IO [GitHub.CheckRun]
    handler n ex = print ("ERROR: " ++ show n ++ " triggert exception " ++ displayException ex) >> pure ([] :: [GitHub.CheckRun])

handleEvalStarted ::
  Connection ->
  Text ->
  Hydra.JobSetId ->
  IO [GitHub.CheckRun]
handleEvalStarted conn host jid = do
  (proj, name, flake, triggertime) <- DB.fetchJobsetBasic conn jid
  Text.putStrLn $ "Eval Started (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow flake
  withGithubFlake flake $ \owner repo hash ->
    pure $
      singleton $
        GitHub.CheckRun owner repo $
          GitHub.CheckRunPayload
            { name = "ci/eval",
              headSha = hash,
              detailsUrl = Just $ "https://" <> host <> "/jobset/" <> proj <> "/" <> name,
              externalId = Just $ tshow jid,
              status = GitHub.InProgress,
              conclusion = Nothing,
              -- `triggertime` is `Nothing` if the evaluation is cached.
              startedAt = (triggertime :: Maybe Int) >>= Just . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral,
              completedAt = Nothing,
              output = Nothing
            }

handleEvalDone ::
  Connection ->
  Text ->
  FilePath ->
  Hydra.JobSetId ->
  Hydra.EvalId ->
  Text ->
  IO [GitHub.CheckRun]
handleEvalDone conn host stateDir jid eid eventName = do
  (proj, name, flake, errmsg, fetcherrmsg, _) <- DB.fetchJobsetErrors conn jid
  (flake', timestamp, checkouttime, evaltime) <- DB.fetchJobsetEval conn eid
  Text.putStrLn $ "Eval " <> eventName <> " (" <> tshow jid <> ", " <> tshow eid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake <> " eval for: " <> flake'
  withGithubFlake flake' $ \owner repo hash -> do
    let evalStatuses =
          mkEvalStatuses
            eid
            owner
            repo
            hash
            host
            timestamp
            checkouttime
            evaltime
            errmsg
            fetcherrmsg

    -- Hydra doesn't send build_finished notifications for cached evals, so we fetch each
    -- of these builds and submit a status current jobset's flake URL
    rows <- DB.fetchCachedEvalBuilds conn eid jid
    buildStatuses <-
      mapM
        ( \(bid, job, status) ->
            handleBuildDone conn host stateDir bid job status True owner repo hash
        )
        rows
    pure $ evalStatuses ++ concat buildStatuses

mkEvalStatuses ::
  Hydra.EvalId ->
  Text ->
  Text ->
  Text ->
  Text ->
  Int ->
  Int ->
  Int ->
  Maybe Text ->
  Maybe Text ->
  [CheckRun]
mkEvalStatuses evalId owner repo hash host startTime checkoutTime evalTime errMsg fetchErrMsg =
  let startedAt = posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral startTime
      fetchCompletedAt = addUTCTime (fromIntegral checkoutTime) startedAt
      evalCompletedAt = addUTCTime (fromIntegral evalTime) fetchCompletedAt
      summary =
        mkEvalDurationSummary
          checkoutTime
          (if isNothing fetchErrMsg then Just evalTime else Nothing)
   in case (errMsg, fetchErrMsg) of
        (Just err, _)
          | not (Text.null err) ->
              singleton (mkEvalErrorStatus startedAt evalCompletedAt summary err)
                -- Creates a failed check run for each job that failed to evaluate.
                -- This is temporarily disabled (by simply passing an empty string)
                -- because there is no way to get rid of these later when the eval
                -- succeeds on a retry, confusing everyone.
                ++ mkFailedJobEvals startedAt evalCompletedAt summary ""
        (_, Just err)
          | not (Text.null err) ->
              [mkFetchErrorStatus startedAt fetchCompletedAt summary err]
        _ -> [mkEvalSuccessStatus startedAt evalCompletedAt summary]
  where
    mkEvalDurationSummary :: Int -> Maybe Int -> Text
    mkEvalDurationSummary checkouttime evaltime =
      "Checkout took "
        <> humanReadableDuration (fromIntegral checkouttime * oneSecond)
        <> "."
        <> maybe mempty (\j -> "\nEvaluation took " <> humanReadableDuration (fromIntegral j * oneSecond) <> ".") evaltime

    mkEvalErrorStatus startedAt completedAt summary err =
      GitHub.CheckRun owner repo $
        GitHub.CheckRunPayload
          { name = "ci/eval",
            headSha = hash,
            detailsUrl = Just $ "https://" <> host <> "/eval/" <> tshow evalId <> "#tabs-errors",
            externalId = Just $ tshow evalId,
            status = GitHub.Completed,
            conclusion = Just GitHub.Failure,
            startedAt = Just startedAt,
            completedAt = Just completedAt,
            output =
              Just $
                GitHub.CheckRunOutput
                  { title = "Evaluation has errors",
                    summary = summary,
                    text = mkEvalErrorSummary err
                  }
          }

    mkFailedJobEvals startedAt completedAt summary err =
      parseFailedJobEvals err <&> \(job, msg) ->
        GitHub.CheckRun owner repo $
          GitHub.CheckRunPayload
            { name = "ci/eval:" <> job,
              headSha = hash,
              detailsUrl = Just $ "https://" <> host <> "/eval/" <> tshow evalId <> "#tabs-errors",
              externalId = Just $ tshow evalId,
              status = GitHub.Completed,
              conclusion = Just GitHub.Failure,
              startedAt = Just startedAt,
              completedAt = Just completedAt,
              output =
                Just $
                  GitHub.CheckRunOutput
                    { title = "Evaluation failed",
                      summary = summary,
                      text = mkEvalErrorSummary msg
                    }
            }

    mkFetchErrorStatus startedAt completedAt summary err =
      GitHub.CheckRun owner repo $
        GitHub.CheckRunPayload
          { name = "ci/eval",
            headSha = hash,
            detailsUrl = Just $ "https://" <> host <> "/eval/" <> tshow evalId <> "#tabs-errors",
            externalId = Just $ tshow evalId,
            status = GitHub.Completed,
            conclusion = Just GitHub.Failure,
            startedAt = Just startedAt,
            completedAt = Just completedAt,
            output =
              Just $
                GitHub.CheckRunOutput
                  { title = "Failed to fetch",
                    summary = summary,
                    text = mkFetchErrorSummary err
                  }
          }

    mkEvalSuccessStatus startedAt completedAt summary =
      GitHub.CheckRun owner repo $
        GitHub.CheckRunPayload
          { name = "ci/eval",
            headSha = hash,
            detailsUrl = Just $ "https://" <> host <> "/eval/" <> tshow evalId,
            externalId = Just $ tshow evalId,
            status = GitHub.Completed,
            conclusion = Just GitHub.Success,
            startedAt = Just startedAt,
            completedAt = Just completedAt,
            output =
              Just $
                GitHub.CheckRunOutput
                  { title = "Evaluation succeeded",
                    summary = summary,
                    text = Nothing
                  }
          }

    -- Given an evaluation's error message, returns the jobs that could not be evaluated and their excerpt from the error message.
    parseFailedJobEvals :: Text -> [(Text, Text)]
    parseFailedJobEvals errormsg =
      internal errormsg <&> \(_, job, msg) -> (job, msg)
      where
        internal :: Text -> [(Text, Text, Text)]
        internal rest =
          case rest =~ ("^in job ‘([^’]*)’:$" :: Text) :: (Text, Text, Text, [Text]) of
            (before, _, after, (job : _)) ->
              let next = internal after
                  msg = case next of
                    ((m, _, _) : _) -> m
                    [] -> after
               in singleton (before, job, msg) ++ next
            _ -> []

mkEvalErrorSummary :: Text -> Maybe Text
mkEvalErrorSummary errmsg =
  let limit = 65535
      errmsgLines = Text.lines errmsg
      maxLines = length errmsgLines
      indentPrefix = indentLine ""
   in binarySearch 0 limit $ \numLines ->
        let parts =
              singleton "Evaluation error:\n\n"
                ++ (if numLines < maxLines then ["Last ", tshow numLines, " lines:\n\n"] else [])
                -- making code blocks by indenting instead of triple backticks so they cannot be escaped
                ++ concatMap (\l -> [indentPrefix, l, "\n"]) (takeEnd numLines errmsgLines)
            totalLength = foldr' ((+) . Text.length) 0 parts
         in ( totalLength < limit && numLines < maxLines,
              if totalLength > limit then Nothing else Just . cs $ Text.concat parts
            )

mkFetchErrorSummary :: Text -> Maybe Text
mkFetchErrorSummary fetcherrmsg =
  let limit = 65535
      fetcherrmsgLines = Text.lines fetcherrmsg
      maxLines = length fetcherrmsgLines
      indentPrefix = indentLine ""
   in binarySearch 0 limit $ \numLines ->
        let parts =
              singleton "Fetch error:\n\n"
                ++ (if numLines < maxLines then ["Last ", tshow numLines, " lines:\n\n"] else [])
                -- making code blocks by indenting instead of triple backticks so they cannot be escaped
                ++ concatMap (\l -> [indentPrefix, l, "\n"]) (takeEnd numLines fetcherrmsgLines)
            totalLength = foldr' ((+) . Text.length) 0 parts
         in ( totalLength < limit && numLines < maxLines,
              if totalLength > limit then Nothing else Just . cs $ Text.concat parts
            )

handleEvalFailed ::
  Connection ->
  Text ->
  Hydra.JobSetId ->
  IO [GitHub.CheckRun]
handleEvalFailed conn host jid = do
  (proj, name, flake, errormsg, fetcherrormsg, errortime) <- DB.fetchJobsetErrors conn jid
  Text.putStrLn $ "Eval Failed (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow (parseGitHubFlakeURI flake)
  withGithubFlake flake $ \owner repo hash ->
    pure $
      singleton $
        GitHub.CheckRun owner repo $
          GitHub.CheckRunPayload
            { name = "ci/eval",
              headSha = hash,
              detailsUrl = Just $ "https://" <> host <> "/jobset/" <> proj <> "/" <> name,
              externalId = Just $ tshow jid,
              status = GitHub.Completed,
              conclusion = Just GitHub.Failure,
              startedAt = Nothing, -- Hydra does not record this information but GitHub still has it
              completedAt = posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral <$> errortime,
              output =
                Just $
                  GitHub.CheckRunOutput
                    { title = "Evaluation failed",
                      summary = "",
                      text =
                        maybe
                          (errormsg >>= mkEvalErrorSummary)
                          mkFetchErrorSummary
                          fetcherrormsg
                    }
            }

handleBuildQueued ::
  Connection ->
  Text ->
  Hydra.BuildId ->
  IO [GitHub.CheckRun]
handleBuildQueued conn host bid = do
  (proj, name, flake, job, desc) <- DB.fetchBuildBasic conn bid
  Text.putStrLn $ "Build Queued (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
  steps <- DB.fetchRecentBuildSteps conn bid
  let prevStepStatus
        | length steps >= 2 = (<&> toEnum) $ steps !! 1
        | otherwise = Nothing
  whenStatusOrJob Nothing prevStepStatus job $ withGithubFlake flake $ \owner repo hash ->
    pure $
      singleton $
        GitHub.CheckRun owner repo $
          GitHub.CheckRunPayload
            { name = "ci/hydra-build:" <> job,
              headSha = hash,
              detailsUrl = Just $ "https://" <> host <> "/build/" <> tshow bid,
              externalId = Just $ tshow bid,
              status = GitHub.Queued,
              conclusion = Nothing,
              startedAt = Nothing,
              completedAt = Nothing,
              output = Nothing
            }

handleBuildStarted ::
  Connection ->
  Text ->
  Hydra.BuildId ->
  IO [GitHub.CheckRun]
handleBuildStarted conn host bid = do
  (proj, name, flake, job, desc, starttime) <- DB.fetchBuildStarted conn bid
  Text.putStrLn $ "Build Started (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
  steps <- DB.fetchRecentBuildSteps conn bid
  let prevStepStatus
        | length steps >= 2 = (<&> toEnum) $ steps !! 1
        | otherwise = Nothing
  whenStatusOrJob Nothing prevStepStatus job $ withGithubFlake flake $ \owner repo hash ->
    pure $
      singleton $
        GitHub.CheckRun owner repo $
          GitHub.CheckRunPayload
            { name = "ci/hydra-build:" <> job,
              headSha = hash,
              detailsUrl = Just $ "https://" <> host <> "/build/" <> tshow bid,
              externalId = Just $ tshow bid,
              status = GitHub.InProgress,
              conclusion = Nothing,
              -- apparently hydra may send the notification before actually starting the build... got 9 seconds difference when testing!
              startedAt = (starttime :: Maybe Int) >>= Just . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral,
              completedAt = Nothing,
              output = Nothing
            }

handleBuildFinished ::
  Connection ->
  Text ->
  FilePath ->
  Hydra.BuildId ->
  [Hydra.BuildId] ->
  IO [GitHub.CheckRun]
handleBuildFinished conn host stateDir bid depBids = do
  -- note; buildstatus is only != NULL for Finished, Queued and Started leave it as NULL.
  (proj, name, flake, job, desc, finished, status) <- DB.fetchBuildFinished conn bid
  Text.putStrLn $ "Build Finished (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
  withGithubFlake flake $ \owner repo hash -> do
    checkRun <- handleBuildDone conn host stateDir bid job status (finished == (1 :: Int)) owner repo hash
    depCheckRuns <-
      sequence $
        (if toEnum status /= Hydra.Succeeded then depBids else []) <&> \depBid -> do
          (depJob, depStatus, depFinished) <- DB.fetchBuildStatus conn depBid
          handleBuildDone conn host stateDir depBid depJob depStatus (depFinished == (1 :: Int)) owner repo hash
    return $ checkRun ++ concat depCheckRuns

handleBuildDone ::
  Connection ->
  Text ->
  FilePath ->
  Hydra.BuildId ->
  Text ->
  Int ->
  Bool ->
  Text ->
  Text ->
  Text ->
  IO [GitHub.CheckRun]
handleBuildDone conn host stateDir bid job status finished owner repo hash = do
  let buildStatus = toEnum status
  let ghCheckRunConclusion
        | finished = toCheckRunConclusion buildStatus
        | otherwise = GitHub.Failure
  steps <- DB.fetchBuildSteps conn bid
  let prevStepStatus
        | length steps >= 2 = (\(_, _, statusInt) -> statusInt <&> toEnum) $ steps !! 1
        | otherwise = Nothing
  whenStatusOrJob (Just ghCheckRunConclusion) prevStepStatus job $ do
    buildTimes <- getBuildTimes
    let failedSteps =
          filter
            (\(_, _, statusInt) -> maybe False ((/= Hydra.Succeeded) . toEnum) statusInt)
            steps
    failedStepLogs <-
      mapM
        ( \(stepnr, drvpath, _) -> do
            logs <-
              catch @SomeException (DB.readBuildLog stateDir drvpath) $ \err -> do
                Text.putStrLn $ "Warning: could not fetch logs: " <> Text.show err
                pure Nothing
            pure (stepnr, drvpath, logs)
        )
        failedSteps
    output <- DB.fetchBuildOutput conn bid
    pure $
      singleton $
        GitHub.CheckRun owner repo $
          GitHub.CheckRunPayload
            { name = "ci/hydra-build:" <> job,
              headSha = hash,
              detailsUrl = Just $ "https://" <> host <> "/build/" <> tshow bid,
              externalId = Just $ tshow bid,
              status = GitHub.Completed,
              conclusion = Just ghCheckRunConclusion,
              startedAt = buildTimes >>= Just . fst,
              completedAt = buildTimes >>= Just . snd,
              output =
                Just $
                  GitHub.CheckRunOutput
                    { title = tshow buildStatus,
                      summary = mkCheckSummary output failedSteps buildStatus,
                      text = mkCheckText failedStepLogs buildStatus
                    }
            }
  where
    getBuildTimes :: IO (Maybe (UTCTime, UTCTime))
    getBuildTimes = do
      buildTimes <- DB.fetchActualBuildTimes conn bid
      pure $ case buildTimes of
        Just (starttime, stoptime) ->
          Just
            ( posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral starttime,
              posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral stoptime
            )
        Nothing -> Nothing

    mkCheckSummary output failedSteps = \case
      Hydra.Succeeded ->
        -- TODO: This is only the "out" path, maybe we do want to put _all_ paths in here JSON encoded?
        -- The idea is that on successful builds, we can grab the nix paths (if needed) directly out of the
        -- github status. And use it for nix-store -r, or similar.
        fromMaybe "" output
      _ -> tshow (length failedSteps) <> " failed steps"

    mkCheckText :: [(Int, String, Maybe Text)] -> BuildStatus -> Maybe Text
    mkCheckText failedStepLogs = \case
      Hydra.Succeeded -> Nothing
      _ ->
        binarySearch 0 maxTextLength $ \numLines ->
          let maxLines = foldr' max 0 $ failedStepLogs <&> \(_, _, logs) -> maybe 0 Text.length logs
              indentPrefix = cs $ indentLine ""
              stepLogsLines = failedStepLogs <&> \(stepnr, drvpath, logs) -> (stepnr, drvpath, logs >>= Just . Text.lines)
              parts :: [Text]
              parts =
                singleton "# Failed Steps\n\n"
                  <> intercalate
                    (singleton "\n")
                    ( stepLogsLines <&> \(stepnr, drvpath, logLines) ->
                        [ "## Step ",
                          Text.show stepnr,
                          "\n\n",
                          -- making code blocks by indenting instead of triple backticks so they cannot be escaped
                          "### Derivation\n\n",
                          indentPrefix,
                          Text.pack drvpath,
                          "\n\n",
                          "### Log\n\n"
                        ]
                          <> (if numLines < maxLines then ["Last ", Text.show numLines, " lines:\n\n"] else [])
                          <> maybe
                            (singleton "*Not available.*\n")
                            ((concatMap (\l -> [indentPrefix, l, "\n"])) . (takeEnd numLines))
                            logLines
                    )
              totalLength = foldr' ((+) . Text.length) 0 parts
           in ( totalLength < maxTextLength && numLines < maxLines,
                if totalLength > maxTextLength then Nothing else Just . cs $ mconcat parts
              )

    maxTextLength = 65535

statusHandler ::
  Text ->
  ByteString ->
  [(String, TokenLease)] ->
  GitHub.CheckRun ->
  IO (Either SomeException Value)
statusHandler ghEndpointUrl ghUserAgent ghToken checkRun = do
  Text.putStrLn $
    "SENDING ["
      <> checkRun.owner
      <> "/"
      <> checkRun.repo
      <> "/"
      <> checkRun.payload.headSha
      <> "] "
      <> checkRun.payload.name
      <> ":"
      <> Text.pack (show checkRun.payload.status)

  let token' = case [tok.token | (owner, tok) <- ghToken, Text.pack owner == checkRun.owner] of
        [t] -> Just t
        _ -> throw (toException $ userError ("No GitHub token found for " <> Text.unpack checkRun.owner))
  let githubSettings =
        GitHubSettings
          { token = token',
            userAgent = ghUserAgent,
            apiVersion = GitHub.gitHubApiVersion
          }

  try $
    GitHub.runGitHubRestT githubSettings ghEndpointUrl $
      queryGitHub
        GHEndpoint
          { method = POST,
            endpoint = "/repos/:owner/:repo/check-runs",
            endpointVals =
              [ "owner" := checkRun.owner,
                "repo" := checkRun.repo
              ],
            ghData = GitHub.toKeyValue checkRun.payload
          }

toCheckRunConclusion :: BuildStatus -> CheckRunConclusion
toCheckRunConclusion = \case
  Hydra.Succeeded -> GitHub.Success
  Hydra.Failed -> GitHub.Failure
  Hydra.DependencyFailed -> GitHub.Failure
  Hydra.Aborted -> GitHub.Cancelled
  Hydra.Cancelled -> GitHub.Cancelled
  Hydra.FailedWithOutput -> GitHub.Failure
  Hydra.TimedOut -> GitHub.TimedOut
  Hydra.LogLimitExceeded -> GitHub.Failure
  Hydra.OutputSizeLimitExceeded -> GitHub.Failure
  Hydra.NonDeterministicBuild -> GitHub.Failure
  Hydra.Other -> GitHub.Failure
