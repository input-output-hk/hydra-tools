{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Concurrent.Async as Async
import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.IORef (newIORef, IORef)
import Data.List (find)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock (NominalDiffTime)
import Database.PostgreSQL.Simple
import Lib.Bridge (notificationWatcher, statusHandlers)
import Lib.GitHub qualified as GitHub
import System.Environment (getEnv, lookupEnv)
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stderr,
    stdin,
    stdout,
  )
import Data.ByteString.Char8 (ByteString)

fetchGitHubTokens :: Int -> FilePath -> Text -> BS.ByteString -> IO [(String, GitHub.TokenLease)]
fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent = do
  putStrLn "Fetching GitHub App installations..."
  ghAppInstalls <- GitHub.fetchInstallations ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent
  putStrLn $ "Found " <> show (length ghAppInstalls) <> " installations"
  forM_ ghAppInstalls $ \(owner, installId) -> do
    Text.putStrLn $ "\t- " <> owner <> " (" <> Text.pack (show installId) <> ")"

  forM ghAppInstalls $ \(owner, installId) -> do
    lease <- GitHub.fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent installId
    Text.putStrLn $ "Fetched new GitHub App installation token valid for " <> owner <> " until " <> Text.pack (show lease.expiry)
    return (Text.unpack owner, lease)

getValidGitHubToken ::
  IORef [(String, GitHub.TokenLease)] -> 
  Text ->
  ByteString ->
  IO [(String, GitHub.TokenLease)]
getValidGitHubToken ghTokens ghEndpointUrl ghUserAgent =
  let buffer = 5 :: NominalDiffTime
   in GitHub.getValidToken buffer ghTokens $ \owner -> do
        putStrLn $ "GitHub token expired or will expire within the next " <> show buffer <> ", fetching a new one..."
        ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
        ghAppInstallIds <- getEnv "GITHUB_APP_INSTALL_IDS" >>= return . read @[(String, Int)]
        let ghAppInstallId = fmap snd . find ((owner ==) . fst) $ ghAppInstallIds
        ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
        maybe
          (error $ "No configured GitHub App Installation ID " <> owner)
          (GitHub.fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent)
          ghAppInstallId

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Read environment variables
  host <- maybe "localhost" id <$> lookupEnv "HYDRA_HOST"
  db <- maybe "localhost" id <$> lookupEnv "HYDRA_DB"
  user <- maybe mempty id <$> lookupEnv "HYDRA_USER"
  pass <- maybe mempty id <$> lookupEnv "HYDRA_PASS"
  stateDir <- getEnv "HYDRA_STATE_DIR"
  ghEndpointUrl <- Text.pack . maybe "https://api.github.com" id <$> lookupEnv "GITHUB_ENDPOINT_URL"
  ghUserAgent <- maybe "hydra-github-bridge" cs <$> lookupEnv "GITHUB_USER_AGENT"

  -- Authenticate to GitHub
  ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
  ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
  -- ghTokens is basically [(String, Token)]
  ghTokens <- fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent >>= newIORef

  -- Start the app loop
  let numWorkers = 10 -- default number of workers
      getValidGitHubToken' = getValidGitHubToken ghTokens ghEndpointUrl ghUserAgent
  eres <-
    Async.race
      ( Async.replicateConcurrently_
          numWorkers
          ( withConnect 
              (ConnectInfo db 5432 user pass "hydra") 
              (statusHandlers ghEndpointUrl ghUserAgent getValidGitHubToken')
          )
      )
      ( withConnect
          (ConnectInfo db 5432 user pass "hydra")
          (notificationWatcher host stateDir)
      )
  either
    (const . putStrLn $ "statusHandler exited")
    (const . putStrLn $ "withConnect exited")
    eres
