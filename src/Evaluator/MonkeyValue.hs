module Evaluator.MonkeyValue (MonkeyValue, MonkeyValueObj (..)) where

class MonkeyValue a

data MonkeyValueObj
  = MonkeyInt Int
  | MonkeyBool Bool
  | MonkeyNull
  deriving (Show)