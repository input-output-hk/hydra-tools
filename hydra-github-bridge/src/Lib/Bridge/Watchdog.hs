{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | There's an issue in production that happens every few days where all processing
-- stalls (notifications, webhooks, and webserver), but the process is still alive. During
-- this time no log messages are output and the only fix is to restart the process, and
-- everything continues normally.
--
-- The purpose of this module is to diagnose this and other concurrency problems. This
-- defines a Watchdog that each thread needs to kick on a regular interval. The watchdog
-- will then report the heartbeat ages and other diagnostics regularly.
module Lib.Bridge.Watchdog
  ( WatchdogEnv (..),
    Heartbeats (..),
    mkWatchdogEnv,
    updateHeartbeat,
    watchdog,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..), SomeException, try)
import Control.Monad (forever)
import Data.Bifunctor (Bifunctor (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (intersperse)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time qualified as Time
import Database.PostgreSQL.Simple (Connection, Only (..), query_)
import Lib.GitHub.WebHookServer (HealthEndpointAPI)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (ClientEnv, ClientM)
import Servant.Client qualified as Servant
import System.Timeout (timeout)

data WatchdogEnv
  = WatchdogEnv
  { -- | How often the watchdog runs
    watchdogInterval :: NominalDiffTime,
    -- | Heartbeats with their last-updated time
    watchdogHeartbeats :: Heartbeats,
    -- | How long between heartbeats do we consider a thread idle
    watchdogIdleThreshold :: NominalDiffTime,
    -- | Bridge API HTTP client
    watchdogBridgeClient :: ClientEnv
  }

-- | A mapping of thread names to latest updated timestamps
newtype Heartbeats = Heartbeats {unHeartbeats :: IORef (Map Text UTCTime)}

mkWatchdogEnv :: NominalDiffTime -> NominalDiffTime -> Int -> IO WatchdogEnv
mkWatchdogEnv interval idleThreshold port = do
  heartbeats <- Heartbeats <$> newIORef Map.empty
  webClient <- bridgeClientEnv port

  pure
    WatchdogEnv
      { watchdogInterval = interval,
        watchdogHeartbeats = heartbeats,
        watchdogIdleThreshold = idleThreshold,
        watchdogBridgeClient = webClient
      }

updateHeartbeat :: Heartbeats -> Text -> IO ()
updateHeartbeat (Heartbeats r) name = do
  now <- Time.getCurrentTime
  atomicModifyIORef' r $ \m -> (Map.insert name now m, ())

bridgeClientEnv :: Int -> IO ClientEnv
bridgeClientEnv port = do
  mgr <- newManager defaultManagerSettings
  let url = Servant.BaseUrl Servant.Http "127.0.0.1" port ""
  pure $ Servant.mkClientEnv mgr url

health :: ClientM (Maybe Text)
health = Servant.client (Proxy @HealthEndpointAPI)

watchdog :: WatchdogEnv -> Connection -> IO ()
watchdog env conn = forever $ do
  threadDelay (toMicros env.watchdogInterval)

  reportHeartbeats env.watchdogIdleThreshold env.watchdogHeartbeats
  reportDiagnostics env.watchdogBridgeClient conn
  where
    toMicros = floor . (* 1_000_000) . Time.nominalDiffTimeToSeconds

reportHeartbeats :: NominalDiffTime -> Heartbeats -> IO ()
reportHeartbeats idleThreshold (Heartbeats heartbeats) = do
  now <- Time.getCurrentTime
  hbs <- readIORef heartbeats

  let hbsText =
        "Heartbeats " <> showMap (Map.map (Time.diffUTCTime now) hbs)

  Text.putStrLn $
    "watchdog tick: " <> hbsText
  where
    showSeconds :: NominalDiffTime -> Text
    showSeconds diff =
      cs (Time.formatTime Time.defaultTimeLocale "%ss" diff)
        <> if diff > idleThreshold
          then
            " [idle]"
          else ""

    showMap m = "{" <> showMapEntries m <> "}"

    showMapEntries =
      Text.concat
        . intersperse ", "
        . Map.foldrWithKey (\k a b -> (k <> " = " <> showSeconds a) : b) []

reportDiagnostics :: ClientEnv -> Connection -> IO ()
reportDiagnostics client conn = do
  -- Check the database connection
  dbCheck <- try' $ query_ @(Only Int) conn "SELECT 1"
  let (dbRes, dbCtx) = evalDbCheck dbCheck

  -- Check the web server
  webCheck <- try' $ Servant.runClientM health client
  let webCheck' =
        -- Flatten `Either SomeException (Maybe (Either ClientError t))` to
        -- `Either SomeException (Maybe t)`
        mapM (first toException) =<< webCheck
      (webRes, webCtx) = evalWebCheck webCheck'

  -- Log the results
  Text.putStrLn $ "watchdog check: DB=" <> dbRes <> ", webServer=" <> webRes
  case dbCtx of
    Just ctx -> Text.putStrLn $ "watchdog db: " <> ctx
    Nothing -> pure ()
  case webCtx of
    Just ctx -> Text.putStrLn $ "watchdog webServer: " <> ctx
    Nothing -> pure ()
  where
    try' :: IO a -> IO (Either SomeException (Maybe a))
    try' = try @SomeException . timeout 5_000_000

    evalDbCheck (Right (Just [Only 1])) = ("OK", Nothing)
    evalDbCheck (Right (Just unexpected)) = ("INVALID", Just $ Text.show unexpected)
    evalDbCheck (Right Nothing) = ("TIMEOUT", Nothing)
    evalDbCheck (Left err) = ("ERROR", Just $ Text.show err)

    evalWebCheck (Right (Just (Just "OK"))) = ("OK", Nothing)
    evalWebCheck (Right (Just res)) = ("INVALID", Just $ Text.show res)
    evalWebCheck (Right Nothing) = ("TIMEOUT", Nothing)
    evalWebCheck (Left err) = ("ERROR", Just $ Text.show err)
