{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Hydra.DB
  ( Command (..),
    readCommand,
    writeCommand,
    ensureIndexes,
    pruneStaleNotifications,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query_)
import GHC.Generics (Generic)
import Lib.Hydra.Client (HydraJobset)

-- The following table exists in the database
--
-- CREATE TABLE IF NOT EXISTS github_commands (
--     id SERIAL PRIMARY KEY,
--     command JSONB NOT NULL,
--     created TIMESTAMP DEFAULT NOW(),
--     processed TIMESTAMP DEFAULT NULL
-- );
data Command
  = UpdateJobset Text Text Text HydraJobset -- only update it, never create
  | CreateOrUpdateJobset Text Text Text HydraJobset -- create or update.
  | DeleteJobset Text Text
  | EvaluateJobset Text Text Bool
  | RestartBuild Int
  deriving (Eq, Generic, Read, Show)

instance ToJSON Command

instance FromJSON Command

readCommand :: Connection -> IO Command
readCommand conn = do
  query_ conn "SELECT id, command FROM github_commands WHERE processed IS NULL ORDER BY created LIMIT 1" >>= \case
    [] -> threadDelay 10_000_000 >> readCommand conn -- 10 sec" \
    [(_id, cmd)] -> do
      void $ execute conn "UPDATE github_commands SET processed = NOW() WHERE id = ?" (Only _id :: Only Int)
      case (Aeson.fromJSON cmd) of
        Aeson.Error e -> error $ show cmd ++ " readCommand: " ++ e
        Aeson.Success x -> return x
    x -> error $ "readCommand: " ++ show x

writeCommand :: Connection -> Command -> IO ()
writeCommand conn cmd = do
  void $ execute conn "INSERT INTO github_commands (command) VALUES (?)" (Only (Aeson.toJSON cmd))

-- | Ensure the partial index on @github_status_payload@ used by the
-- unsent-payload query exists.  Must be called on a dedicated autocommit
-- connection because @CREATE INDEX CONCURRENTLY@ cannot run inside a
-- transaction.
ensureIndexes :: Connection -> IO ()
ensureIndexes conn = do
  void $
    execute_
      conn
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS \
      \idx_github_status_payload_unsent \
      \ON github_status_payload (status_id, id DESC) \
      \WHERE sent IS NULL AND tries < 5"

-- | Mark stale unsent payloads as sent when a newer payload for the same
-- @(owner, repo, name)@ has already been successfully delivered.  Returns
-- the number of rows updated.
pruneStaleNotifications :: Connection -> IO Int64
pruneStaleNotifications conn =
  execute_
    conn
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
