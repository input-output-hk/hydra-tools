module Main where

import qualified Lib.Bridge.HydraToGitHubSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec Lib.Bridge.HydraToGitHubSpec.spec
