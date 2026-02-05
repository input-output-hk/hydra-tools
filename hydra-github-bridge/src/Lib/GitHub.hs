{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib.GitHub
  ( module Lib.GitHub.Client,
    CheckRun(..),
    parseGitHubFlakeURI,
    TokenLease (..),
    fetchInstallations,
    fetchAppInstallationToken,
    getValidToken,
  )
where

import Control.Monad (forM)
import Control.Monad.IO.Class
import Data.Aeson hiding
  ( Error,
    KeyValue,
    Success,
    (.:),
  )
import Data.ByteString.Char8 qualified as BS
import Data.IORef (IORef, readIORef, writeIORef)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock
  ( NominalDiffTime,
    addUTCTime,
    getCurrentTime,
  )
import Lib.GitHub.Client 
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GitHub.REST
  ( GHEndpoint (..),
    GitHubSettings (..),
    KeyValue ((:=)),
    MonadGitHubREST (..),
    StdMethod (GET, POST),
    Token (BearerToken),
    queryGitHub,
    (.:),
  )
import GitHub.REST.Auth (getJWTToken)

-- TODO[sgillespie]: The rest of these seem to have business logic, so we'll figure it out
-- later
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

fetchInstallations :: Text -> Int -> FilePath -> BS.ByteString -> IO [(Text, Int)]
fetchInstallations ghEndpointUrl appId appKeyFile ghUserAgent = do
  signer <- loadSigner appKeyFile
  jwt <- getJWTToken signer appId

  let githubSettings =
        GitHubSettings
          { token = Just jwt,
            userAgent = ghUserAgent,
            apiVersion = gitHubApiVersion
          }
  response <-
    liftIO $
      runGitHubRestT githubSettings ghEndpointUrl $
        queryGitHub
          GHEndpoint
            { method = GET,
              endpoint = "/app/installations",
              endpointVals = [],
              ghData = []
            }

  return $
    map
      ( \inst ->
          let account = inst .: "account" :: Value
           in (account .: "login", inst .: "id")
      )
      response

fetchAppInstallationToken :: Text -> Int -> FilePath -> BS.ByteString -> Int -> IO TokenLease
fetchAppInstallationToken ghEndpointUrl appId appKeyFile ghUserAgent appInstallationId = do
  signer <- loadSigner appKeyFile
  jwt <- getJWTToken signer appId

  let githubSettings =
        GitHubSettings
          { token = Just jwt,
            userAgent = ghUserAgent,
            apiVersion = gitHubApiVersion
          }
  response <-
    liftIO $
      runGitHubRestT githubSettings ghEndpointUrl $
        queryGitHub
          GHEndpoint
            { method = POST,
              endpoint = "/app/installations/:appInstallId/access_tokens",
              endpointVals = ["appInstallId" := appInstallationId],
              ghData =
                [ "permissions"
                    := [ "checks" := ("write" :: String),
                         "statuses" := ("write" :: String)
                       ]
                ]
            }

  expiry <- iso8601ParseM (response .: "expires_at" :: String)

  return $
    TokenLease
      { token = BearerToken $ cs (response .: "token" :: String),
        expiry = Just expiry
      }

getValidToken :: NominalDiffTime -> IORef [(String, TokenLease)] -> (String -> IO TokenLease) -> IO [(String, TokenLease)]
getValidToken buffer lease fetch = do
  leases' <- readIORef lease
  now <- getCurrentTime
  leases'' <- forM leases' $ \(owner, tok) -> do
    case tok.expiry of
      Just expiry | addUTCTime buffer now < expiry -> return (owner, tok)
      _ -> (owner,) <$> fetch owner
  writeIORef lease leases''
  return leases''
