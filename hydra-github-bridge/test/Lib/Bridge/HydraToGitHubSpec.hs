{-# LANGUAGE OverloadedStrings #-}

module Lib.Bridge.HydraToGitHubSpec (spec) where

import Lib.Bridge.HydraToGitHub
  ( EmitDecision (..),
    decideEmit,
    hasRequiredKeyword,
  )
import Lib.GitHub.Client
  ( CheckRunConclusion (..),
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "hasRequiredKeyword" $ do
    it "matches the bare word as a prefix" $
      hasRequiredKeyword "required" `shouldBe` True
    it "matches as a hyphenated prefix" $
      hasRequiredKeyword "required-aarch64-linux" `shouldBe` True
    it "matches as a hyphenated suffix" $
      hasRequiredKeyword "build-required" `shouldBe` True
    it "matches as a dot-delimited component" $
      hasRequiredKeyword "x86_64-linux.required.foo" `shouldBe` True
    it "matches `nonrequired` as a prefix" $
      hasRequiredKeyword "nonrequired" `shouldBe` True
    it "matches `nonrequired` as a dot-delimited component" $
      hasRequiredKeyword "foo.nonrequired.bar" `shouldBe` True
    it "does not match an unrelated job name" $
      hasRequiredKeyword "build" `shouldBe` False
    it "does not match a name that only embeds the substring mid-word" $
      hasRequiredKeyword "foo-not-required-yet" `shouldBe` False

  describe "decideEmit" $ do
    let nonRequiredJob = "x86_64-linux.build"
    let requiredJob = "x86_64-linux.required.build"

    it "emits unconditionally for required-keyword jobs (no status)" $
      decideEmit Nothing requiredJob `shouldBe` Emit
    it "emits unconditionally for required-keyword jobs (Success)" $
      decideEmit (Just Success) requiredJob `shouldBe` Emit
    it "emits for non-required when current conclusion is Failure" $
      decideEmit (Just Failure) nonRequiredJob `shouldBe` Emit
    it "emits for non-required when current conclusion is Cancelled" $
      decideEmit (Just Cancelled) nonRequiredJob `shouldBe` Emit
    it "emits for non-required when current conclusion is TimedOut" $
      decideEmit (Just TimedOut) nonRequiredJob `shouldBe` Emit
    it "emits for non-required when current conclusion is Stale" $
      decideEmit (Just Stale) nonRequiredJob `shouldBe` Emit
    it "defers to prior-failure check for non-required success" $
      decideEmit (Just Success) nonRequiredJob `shouldBe` CheckPriorFailure
    it "defers to prior-failure check for non-required Neutral" $
      decideEmit (Just Neutral) nonRequiredJob `shouldBe` CheckPriorFailure
    it "defers to prior-failure check for non-required with no status" $
      decideEmit Nothing nonRequiredJob `shouldBe` CheckPriorFailure
