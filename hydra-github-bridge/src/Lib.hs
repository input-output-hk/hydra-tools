{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( module Lib.GitHub.WebHookServer,
    module Lib.Hydra.DB,
    module Lib.Hydra.Client,
    module Lib.Bridge.GitHubToHydra,
    toCheckRunConclusion,
    binarySearch,
    binarySearchM,
  )
where

import Data.Void (absurd)
import qualified Lib.GitHub as GitHub
import Lib.GitHub.WebHookServer
import Lib.Hydra.Client
import Lib.Hydra.DB
import Lib.Bridge.GitHubToHydra
import qualified Lib.Hydra as Hydra

toCheckRunConclusion :: Hydra.BuildStatus -> GitHub.CheckRunConclusion
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

binarySearch :: Int -> Int -> (Int -> (Bool, a)) -> a
binarySearch low high find =
  either absurd id $ binarySearchM low high (Right . find)

binarySearchM :: (Monad m) => Int -> Int -> (Int -> m (Bool, a)) -> m a
binarySearchM low high find = do
  let mid = (low + high) `div` 2
  (higher, found) <- find mid
  if low == high
    then return found
    else
      if higher
        then binarySearchM (mid + 1) high find
        else binarySearchM low mid find
