{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Hydra.DB
  ( Command (..),
    readCommand,
    writeCommand,
    readBuildLog,
    fetchJobsetBasic,
    fetchJobsetErrors,
    fetchJobsetEval,
    fetchCachedEvalBuilds,
    fetchBuildStatus,
    fetchBuildBasic,
    fetchBuildStarted,
    fetchBuildFinished,
    fetchRecentBuildSteps,
    fetchBuildSteps,
    fetchBuildOutput,
    fetchActualBuildTimes,
  )
where

import Codec.Compression.BZip (decompressErr)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LByteString
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Database.PostgreSQL.Simple (Connection, Only (..), Query, execute, query, query_)
import GHC.Generics (Generic)
import Lib.Hydra.Client (BuildId, EvalId, HydraJobset, JobSetId)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName, (<.>), (</>))

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

fetchJobsetBasic ::
  Connection ->
  JobSetId ->
  IO (Text, Text, Text, Maybe Int)
fetchJobsetBasic conn jobsetId = do
  -- We're assuming the jobset exists, will throw if missing
  [res] <-
    query
      conn
      "SELECT project, name, flake, triggertime FROM jobsets WHERE id = ?"
      (Only jobsetId)
  pure res

fetchJobsetErrors ::
  Connection ->
  JobSetId ->
  IO (Text, Text, Text, Maybe Text, Maybe Text, Maybe Int)
fetchJobsetErrors conn jobsetId = do
  -- We're assuming the jobset exists, will throw if missing
  [res] <-
    query
      conn
      "SELECT project, name, flake, errormsg, fetcherrormsg, errortime FROM jobsets WHERE id = ?"
      (Only jobsetId)
  pure res

fetchJobsetEval ::
  Connection ->
  EvalId ->
  IO (Text, Int, Int, Int)
fetchJobsetEval conn evalId = do
  -- We're assuming the jobset exists, will throw if missing
  [res] <-
    query
      conn
      "SELECT flake, timestamp, checkouttime, evaltime FROM jobsetevals WHERE id = ?"
      (Only evalId)
  pure res

-- | Select finished builds of the most recent new evaluation.
--
-- Only selects builds if this evaluation is cached (is identical to a previous one),
-- otherwise no rows are returned. This is necessary because build_finished notifications
-- are not sent by Hydra when an evaluation is cached.
fetchCachedEvalBuilds ::
  Connection ->
  -- | Current (possibly cached) evaluation
  EvalId ->
  -- | JobSet to query
  JobSetId ->
  IO [(Int, Text, Int)]
fetchCachedEvalBuilds conn evalId jobsetId = do
  let q =
        "\
        \WITH prev_jobseteval AS (              \
        \    SELECT *                           \
        \    FROM jobsetevals                   \
        \    WHERE                              \
        \        id < ? AND                     \
        \        jobset_id = ? AND              \
        \        hasnewbuilds = 1               \
        \    ORDER BY id DESC                   \
        \    FETCH FIRST ROW ONLY               \
        \)                                      \
        \SELECT b.id, b.job, b.buildstatus      \
        \FROM builds b                          \
        \JOIN prev_jobseteval e ON NOT EXISTS ( \
        \    SELECT NULL                        \
        \    FROM jobsetevals                   \
        \    WHERE                              \
        \        id = ? AND                     \
        \        hasnewbuilds = 1               \
        \)                                      \
        \JOIN jobsetevalmembers m ON            \
        \    m.build = b.id AND                 \
        \    m.eval = e.id                      \
        \WHERE b.finished = 1                   \
        \ "
  query conn q [evalId, jobsetId, evalId]

fetchBuildStatus ::
  Connection ->
  BuildId ->
  IO (Text, Int, Int)
fetchBuildStatus conn buildId = do
  -- We're assuming the build exists, will throw if missing
  [res] <-
    query
      conn
      "SELECT job, buildstatus, finished FROM builds WHERE id = ?"
      (Only buildId)

  pure res

fetchBuildBasic ::
  Connection ->
  BuildId ->
  IO (Text, Text, Text, Text, Maybe Text)
fetchBuildBasic conn buildId = do
  let q = "SELECT j.project, j.name, e.flake, b.job, b.description" <> queryBuildsFrom

  [res] <- query conn q (Only buildId)
  pure res

queryBuildsFrom :: Query
queryBuildsFrom =
  " FROM builds b"
    <> " JOIN jobsets j ON b.jobset_id = j.id"
    <> " JOIN jobsetevalmembers m ON m.build = b.id"
    <> " JOIN jobsetevals e ON e.id = m.eval"
    <> " WHERE b.id = ?"
    <> " ORDER BY e.id DESC"
    <> " FETCH FIRST ROW ONLY"

fetchBuildStarted ::
  Connection ->
  BuildId ->
  IO (Text, Text, Text, Text, Maybe Text, Maybe Int)
fetchBuildStarted conn buildId = do
  let q =
        "SELECT j.project, j.name, e.flake, b.job, b.description, b.starttime"
          <> queryBuildsFrom

  [res] <- query conn q (Only buildId)
  pure res

fetchBuildFinished ::
  Connection ->
  BuildId ->
  IO (Text, Text, Text, Text, Maybe Text, Int, Int)
fetchBuildFinished conn buildId = do
  let q =
        "SELECT j.project, j.name, e.flake, b.job, b.description, b.finished, b.buildstatus"
          <> queryBuildsFrom

  [res] <- query conn q (Only buildId)
  pure res

fetchRecentBuildSteps ::
  Connection ->
  BuildId ->
  IO [Maybe Int]
fetchRecentBuildSteps conn buildId = do
  res <-
    query
      conn
      "SELECT status FROM buildsteps WHERE build = ? ORDER BY stepnr DESC LIMIT 2"
      (Only buildId)

  pure (map fromOnly res)

fetchBuildSteps ::
  Connection ->
  BuildId ->
  IO [(Int, FilePath, Maybe Int)]
fetchBuildSteps conn buildId =
  query
    conn
    "SELECT stepnr, drvpath, status FROM buildsteps WHERE build = ? ORDER BY stepnr DESC"
    (Only buildId)

fetchBuildOutput ::
  Connection ->
  BuildId ->
  IO (Maybe Text)
fetchBuildOutput conn buildId = do
  res <-
    query
      conn
      "SELECT path FROM buildoutputs WHERE name = 'out' and build = ? LIMIT 1"
      (Only buildId)

  pure $ listToMaybe (map fromOnly res)

fetchActualBuildTimes ::
  Connection ->
  BuildId ->
  IO (Maybe (Int, Int))
fetchActualBuildTimes conn buildId = do
  -- TODO: This query is nightmarishly complex. It would be much more readable/maintainable
  -- to split this into multiple queries and stitch them together in code.
  let q =
        "\
        \WITH                                                          \
        \    given_build AS (                                          \
        \        SELECT *                                              \
        \        FROM builds                                           \
        \        WHERE id = ?                                          \
        \    ),                                                        \
        \    given_build_output AS (                                   \
        \        SELECT o.*                                            \
        \        FROM buildoutputs o                                   \
        \        JOIN given_build g_b ON o.build = g_b.id              \
        \        FETCH FIRST ROW ONLY                                  \
        \    ),                                                        \
        \    actual_build_step AS (                                    \
        \        SELECT s.*                                            \
        \        FROM buildsteps s                                     \
        \        JOIN buildstepoutputs o ON                            \
        \            o.build = s.build AND                             \
        \            o.stepnr = s.stepnr                               \
        \        JOIN given_build_output g_b_o ON o.path = g_b_o.path  \
        \        WHERE s.busy = 0                                      \
        \        ORDER BY s.status, s.stoptime DESC                    \
        \        FETCH FIRST ROW ONLY                                  \
        \    ),                                                        \
        \    actual_build AS (                                         \
        \        SELECT b.*                                            \
        \        FROM builds b                                         \
        \        JOIN actual_build_step a_b_s ON a_b_s.build = b.id    \
        \    ),                                                        \
        \    given_build_maybe AS (                                    \
        \        SELECT *                                              \
        \        FROM given_build                                      \
        \        WHERE                                                 \
        \            finished = 0 OR                                   \
        \            iscachedbuild = 0                                 \
        \    ),                                                        \
        \    selected_build AS (                                       \
        \        SELECT *                                              \
        \        FROM given_build_maybe                                \
        \                                                              \
        \        UNION ALL                                             \
        \                                                              \
        \        SELECT *                                              \
        \        FROM actual_build                                     \
        \        WHERE NOT EXISTS (SELECT NULL FROM given_build_maybe) \
        \    )                                                         \
        \SELECT selected_build.starttime, selected_build.stoptime      \
        \FROM selected_build                                           \
        \ "
  res <- query conn q (Only buildId)
  pure $ case res of
    [(starttime, stoptime)] -> Just (starttime, stoptime)
    _ -> Nothing

readBuildLog :: FilePath -> FilePath -> IO (Maybe Text)
readBuildLog hydraStateDir drvPath = do
  let drvName = takeFileName drvPath
      -- The first two characters of the derivation hash is the directory name and the
      -- rest is the file name
      bucketed = take 2 drvName </> drop 2 drvName
      path = hydraStateDir </> "build-logs" </> bucketed

  -- Attempt to read the drv file
  drvExists <- doesFileExist path
  if drvExists
    then ByteString.readFile path >>= tryDecode
    else do
      -- drv file does not exist, add ".bz2" to the end and try to read it again
      let bz2Path = path <.> "bz2"
      bz2Exists <- doesFileExist bz2Path
      if bz2Exists
        then LByteString.readFile bz2Path >>= tryDecompress >>= tryDecode
        else pure Nothing
  where
    tryDecode = either throwIO (pure . Just) . decodeUtf8'
    tryDecompress = either throwIO (pure . ByteString.toStrict) . decompressErr
