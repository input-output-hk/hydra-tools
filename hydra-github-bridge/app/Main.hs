{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async as Async
import Control.Monad
import Data.ByteString.Char8 qualified as C8
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple
import Lib.Bridge (app, hydraClient, hydraClientEnv, statusHandlers)
import Lib.Bridge.HydraToGitHub
    ( HydraToGitHubEnv(..),
      fetchGitHubTokens,
      runHydraToGitHubT, notificationWatcherWithSSE )
import Lib.GitHub
    ( gitHubKey,
      gitHubKey )
import Lib.Bridge.GitHubToHydra (GitHubToHydraEnv (..), parseInstallIds)
import Lib.Hydra (HydraClientEnv (..))
import Lib.SSE (newStatusCache, runSSEServer)
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv, lookupEnv)
import System.Exit (die)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdin, stdout)
import Data.Time (NominalDiffTime)

getGhAppInstallIds :: IO [(Text, Int)]
getGhAppInstallIds = do
  ghAppInstallIds <- parseInstallIds . Text.pack <$> getEnv "GITHUB_APP_INSTALL_IDS"
  either
    (die . ("Failed to parse " <>))
    pure
    ghAppInstallIds

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Read environment variables
  host <- fromMaybe "localhost" <$> lookupEnv "HYDRA_HOST"
  db <- fromMaybe "localhost" <$> lookupEnv "HYDRA_DB"
  db_user <- fromMaybe mempty <$> lookupEnv "HYDRA_DB_USER"
  db_pass <- fromMaybe mempty <$> lookupEnv "HYDRA_DB_PASS"
  api_user <- maybe mempty Text.pack <$> lookupEnv "HYDRA_USER"
  api_pass <- maybe mempty Text.pack <$> lookupEnv "HYDRA_PASS"
  port <- maybe 8080 read <$> lookupEnv "PORT"
  stateDir <- getEnv "HYDRA_STATE_DIR"
  ghEndpointUrl <- maybe "https://api.github.com" cs <$> lookupEnv "GITHUB_ENDPOINT_URL"
  ghUserAgent <- maybe "hydra-github-bridge" cs <$> lookupEnv "GITHUB_USER_AGENT"
  -- Webhook secret for signature verification.
  -- Prefer GITHUB_WEBHOOK_SECRET, fall back to KEY for backwards compatibility.
  ghKey <- do
    v <- lookupEnv "GITHUB_WEBHOOK_SECRET"
    case v of
      Just k  -> pure (C8.pack k)
      Nothing -> maybe mempty C8.pack <$> lookupEnv "KEY"
  checkRunPrefix <- maybe "ci/hydra-build:" Text.pack <$> lookupEnv "CHECK_RUN_PREFIX"
  filterJobs <- maybe True (\v -> v == "true" || v == "1") <$> lookupEnv "FILTER_JOBS"

  -- SSE configuration
  sseEnabled <- maybe True (\v -> v == "true" || v == "1") <$> lookupEnv "SSE_ENABLED"
  ssePort <- maybe 8812 read <$> lookupEnv "SSE_PORT"
  let sseTtl = 86400 :: NominalDiffTime -- 24 hours

  -- Authenticate to GitHub
  ghAppId <- read <$> getEnv "GITHUB_APP_ID"
  ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
  ghAppInstallIds <- getGhAppInstallIds

  ghTokens <- fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent ghAppInstallIds
  ghTokensRef <- newIORef ghTokens

  putStrLn $ "Connecting to Hydra at " <> host
  env <- hydraClientEnv (Text.pack host) api_user api_pass

  putStrLn $ "Server is starting on port " ++ show port
  when sseEnabled $
    putStrLn $ "SSE server will start on port " ++ show ssePort

  -- Initialize SSE status cache
  cache <- newStatusCache

  -- Start the app loop
  let numWorkers = 10 -- default number of workers
      hydraToGitHubEnv =
        HydraToGitHubEnv
          { htgEnvHydraHost = host,
            htgEnvHydraStateDir = stateDir,
            htgEnvGhAppId = ghAppId,
            htgEnvGhAppKeyFile = ghAppKeyFile,
            htgEnvGhEndpointUrl = ghEndpointUrl,
            htgEnvGhUserAgent = ghUserAgent,
            htgEnvGhAppInstallIds = ghAppInstallIds,
            htgEnvGhTokens = ghTokensRef,
            htgEnvCheckRunPrefix = checkRunPrefix,
            htgEnvFilterJobs = filterJobs
          }

      gitHubToHydraEnv =
        GitHubToHydraEnv
          { gthEnvHydraClient = hceClientEnv env,
            gthEnvGitHubKey = gitHubKey ghKey,
            gthEnvGhAppInstallIds = ghAppInstallIds
          }
  -- Create partial index for the optimized unsent-payload query.
  -- CONCURRENTLY cannot run inside a transaction, so we use a
  -- dedicated connection with autocommit.
  withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \migConn -> do
    putStrLn "Ensuring partial index idx_github_status_payload_unsent exists..."
    _ <- execute_ migConn
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS \
      \idx_github_status_payload_unsent \
      \ON github_status_payload (status_id, id DESC) \
      \WHERE sent IS NULL AND tries < 5"
    putStrLn "Index ready."

  Async.mapConcurrently_
    id $
    [ Async.replicateConcurrently_
        numWorkers
        ( withConnect
            (ConnectInfo db 5432 db_user db_pass "hydra")
            (runHydraToGitHubT hydraToGitHubEnv . statusHandlers)
        ),
      withConnect
        (ConnectInfo db 5432 db_user db_pass "hydra")
        (hydraClient env),
      -- Use the SSE-broadcasting variant of the notification watcher.
      withConnect
        (ConnectInfo db 5432 db_user db_pass "hydra")
        (notificationWatcherWithSSE cache host stateDir checkRunPrefix filterJobs),
      withConnect
        (ConnectInfo db 5432 db_user db_pass "hydra")
        (run port . app gitHubToHydraEnv),
      -- Periodically prune stale notifications for old commits that
      -- have already been superseded by newer evaluations.  Only marks
      -- a payload as sent when a later payload for the same
      -- (owner, repo, name) has already been successfully delivered.
      withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn ->
        forever $ do
          threadDelay (5 * 60 * 1000000) -- 5 minutes
          pruned <- execute_ conn
            "UPDATE github_status_payload SET sent = NOW() \
            \WHERE id IN ( \
            \  SELECT p.id \
            \  FROM github_status_payload p \
            \  JOIN github_status s ON s.id = p.status_id \
            \  WHERE p.sent IS NULL AND p.tries < 5 \
            \    AND EXISTS ( \
            \      SELECT 1 \
            \      FROM github_status_payload p2 \
            \      JOIN github_status s2 ON s2.id = p2.status_id \
            \      WHERE s2.owner = s.owner AND s2.repo = s.repo AND s2.name = s.name \
            \        AND p2.sent IS NOT NULL AND p2.id > p.id \
            \    ) \
            \)"
          when (pruned > 0) $
            putStrLn $ "Pruned " ++ show pruned ++ " stale notification(s)"
    ]
    ++ [ runSSEServer cache ssePort sseTtl | sseEnabled ]
