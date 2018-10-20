module Test where

import           RuleSpec
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  tree <- testSpec "hspec tests" ruleSpecSuite
  defaultMain tree
