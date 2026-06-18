{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Bridge.DB
  ( hasPriorReportedFailure,
  )
where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), query)
import Lib.GitHub qualified as GitHub

-- | True iff the bridge has previously enqueued a failure-class conclusion
-- (see 'GitHub.failureConclusions') for the given commit+job. Used so that
-- once GitHub has been told a job is broken, subsequent transitions
-- (queued/in_progress/success) keep being reported even for jobs whose name
-- does not match the required\/nonrequired keyword.
hasPriorReportedFailure ::
  Connection ->
  -- | repo owner
  Text ->
  -- | repo name
  Text ->
  -- | head sha
  Text ->
  -- | check-run name
  Text ->
  IO Bool
hasPriorReportedFailure conn owner repo headSha name = do
  [Only exists] <-
    query
      conn
      "SELECT EXISTS ( \
      \  SELECT 1 FROM github_status_payload p \
      \  JOIN github_status s ON p.status_id = s.id \
      \  WHERE s.owner = ? AND s.repo = ? AND s.headSha = ? AND s.name = ? \
      \    AND p.payload->>'conclusion' IN ? \
      \)"
      ( owner,
        repo,
        headSha,
        name,
        In (map GitHub.conclusionText GitHub.failureConclusions)
      )
  pure exists
