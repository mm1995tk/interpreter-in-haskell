module Evaluator.MonkeyValue (MonkeyValue (..), MonkeyValueObj (..), isTruthy, unwrap, wrapLitPure) where

data MonkeyValue
  = ReturnValue MonkeyValueObj
  | LiteralValue MonkeyValueObj

data MonkeyValueObj
  = MonkeyInt Int
  | MonkeyBool Bool
  | MonkeyNull
  deriving (Show, Eq)

isTruthy :: MonkeyValueObj -> Bool
isTruthy MonkeyNull = False
isTruthy (MonkeyInt _) = True
isTruthy (MonkeyBool b) = b

unwrap :: MonkeyValue -> MonkeyValueObj
unwrap (ReturnValue v) = v
unwrap (LiteralValue v) = v

wrapLitPure :: (Applicative m) => MonkeyValueObj -> m MonkeyValue
wrapLitPure = pure . LiteralValue