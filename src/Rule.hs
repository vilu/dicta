{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Rule
  ( Predicate
  , Priority(..)
  , Id
  , Type
  , Application
  , Rule(..)
  , ruleId
  , ruleType
  , predicate
  , priority
  , application
  , toPositive
  , evaluate
  )
where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.List
import           Data.Maybe

type Predicate ctx = ctx -> Bool

data Priority = Exclusive | Priority Positive deriving Eq

newtype Positive = Positive { unPositive :: Int } deriving Eq

instance Show Priority where
  show Exclusive               = "Exclusive"
  show (Priority (Positive p)) = "Priority " ++ show p

toPositive :: Int -> Maybe Positive
toPositive i = if i < 0 then Nothing else Just (Positive i)

type Id = String

type Type = String

type Application a = a -> a

data Rule a ctx = Rule {
    _ruleId      :: Id
  , _ruleType    :: Type
  , _predicate   :: Predicate ctx
  , _priority    :: Priority
  , _application :: Application a
}

makeLenses ''Rule

instance Show (Rule a ctx) where
  show r = t ++ "(" ++ i ++ ")" ++ show p
    where
      t = r ^. ruleType
      i = r ^. ruleId
      p = r ^. priority

type Rules a ctx = [Rule a ctx]

type MonadRule ctx = ReaderT ctx (Writer [String])

applyRules :: Show a => a -> Rules a ctx -> MonadRule ctx a
applyRules = foldM applyRule
 where
  applyRule :: Show a => a -> Rule a ctx -> MonadRule ctx a
  applyRule input rule = do
    context <- ask
    let appl        = rule ^. application
        pred        = rule ^. predicate
        shouldApply = pred context
        output      = if shouldApply then appl input else input
    if shouldApply then logRuleApplication input output rule else logIgnoredRule rule
    liftValue output

logRuleApplication :: (MonadWriter [String] m, Show a) => a -> a -> Rule a ctx -> m ()
logRuleApplication input output rule = do
  tell ["Applying Rule " ++ show rule ++ " to " ++ show input ++ " = " ++ show output]
  return ()

logIgnoredRule :: (MonadWriter [String] m) => Rule a ctx -> m ()
logIgnoredRule rule = do
  tell ["Ignoring rule " ++ show rule]
  return ()

liftValue :: a -> MonadRule ctx a
liftValue a = lift $ writer (a, [])

evaluate :: Show a => a -> ctx -> Rules a ctx -> Either String (a, [String])
evaluate input context rules = (\rs -> runWriter (runReaderT (applyRules input rs) context)) <$> preprocessRules rules
 where
  preprocessRules rs = filterRules <$> (validate . sortRules) rs
  sortRules = sortBy ruleOrdering
  validate (Rule _ _ _ Exclusive _ : Rule _ _ _ Exclusive _ : _) = Left "Error: Rule list contains multiple exclusive rules"
  validate rs = Right rs
  filterRules (r@(Rule _ _ _ Exclusive _) : _) = [r]
  filterRules rs                               = rs

ruleOrdering Rule { _priority = Exclusive } Rule { _priority = Exclusive } = EQ
ruleOrdering Rule { _priority = Exclusive } _                              = LT
ruleOrdering _                              Rule { _priority = Exclusive } = GT
ruleOrdering Rule { _priority = (Priority (Positive p1)) } Rule { _priority = Priority (Positive p2) } = compare p1 p2
