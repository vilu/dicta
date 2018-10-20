module RuleSpec where

import           Data.Maybe
import           Rule
import           Test.Tasty
import           Test.Tasty.Hspec

ruleSpecSuite :: Spec
ruleSpecSuite = describe "Rules" $ do
  it "should handle singel plus rule" $ evaluate 10.0 (Context True) [plusRule] `shouldBe` Right
    (20.0, ["Applying Rule plus(plusRule)Priority 20 to 10.0 = 20.0"])
  it "should handle single multiply rule" $ evaluate 10.0 (Context True) [multiplyRule] `shouldBe` Right
    (20.0, ["Applying Rule multiply(multiplyRule)Priority 30 to 10.0 = 20.0"])
  it "should handle single fix rule" $ evaluate 10.0 (Context True) [constRule] `shouldBe` Right
    (50.0, ["Applying Rule const(constRule)Priority 40 to 10.0 = 50.0"])
  it "should handle multiple rules" $ do
    evaluate 10.0 (Context True) [plusRule, multiplyRule]
      `shouldBe` Right (40.0, ["Applying Rule plus(plusRule)Priority 20 to 10.0 = 20.0", "Applying Rule multiply(multiplyRule)Priority 30 to 20.0 = 40.0"])
    evaluate 10.0 (Context True) [multiplyRule, plusRule]
      `shouldBe` Right (40.0, ["Applying Rule plus(plusRule)Priority 20 to 10.0 = 20.0", "Applying Rule multiply(multiplyRule)Priority 30 to 20.0 = 40.0"])
  it "should handle exclusive rules" $ do
    evaluate 10.0 (Context True) [multiplyRule, exclusiveRule1] `shouldBe` Right (30.0, ["Applying Rule multiply(exclusiveRule1)Exclusive to 10.0 = 30.0"])
    evaluate 10.0 (Context True) [exclusiveRule1, multiplyRule] `shouldBe` Right (30.0, ["Applying Rule multiply(exclusiveRule1)Exclusive to 10.0 = 30.0"])
  it "should not allow multiple exclusive rules" $ evaluate 10.0 (Context True) [exclusiveRule1, exclusiveRule2] `shouldBe` Left
    "Error: Rule list contains multiple exclusive rules"
  it "should not apply when the predicate fails" $ evaluate 10.0 (Context False) [plusRule] `shouldBe` Right (10.0, ["Ignoring rule plus(plusRule)Priority 20"])



newtype Context = Context Bool

unsafeMakePriority :: Int -> Priority
unsafeMakePriority i = Priority $ fromJust $ toPositive i

plusRule :: Rule Double Context
plusRule = Rule {_ruleId = "plusRule", _ruleType = "plus", _predicate = \(Context b) -> b, _priority = unsafeMakePriority 20, _application = (+ 10)}

multiplyRule :: Rule Double Context
multiplyRule = Rule {_ruleId = "multiplyRule", _ruleType = "multiply", _predicate = \(Context b) -> b, _priority = unsafeMakePriority 30, _application = (* 2)}

constRule :: Rule Double Context
constRule = Rule {_ruleId = "constRule", _ruleType = "const", _predicate = \(Context b) -> b, _priority = unsafeMakePriority 40, _application = const 50}

ignoreRule :: Rule Double Context
ignoreRule = Rule {_ruleId = "ignoreRule", _ruleType = "plus", _predicate = const False, _priority = unsafeMakePriority 20, _application = (+ 10)}

exclusiveRule1 :: Rule Double Context
exclusiveRule1 = Rule {_ruleId = "exclusiveRule1", _ruleType = "multiply", _predicate = \(Context b) -> b, _priority = Exclusive, _application = (* 3)}

exclusiveRule2 :: Rule Double Context
exclusiveRule2 = Rule {_ruleId = "exclusiveRule2", _ruleType = "multiply", _predicate = \(Context b) -> b, _priority = Exclusive, _application = (* 4)}
