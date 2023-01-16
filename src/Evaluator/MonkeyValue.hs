{-# OPTIONS_GHC -Wno-partial-fields #-}

module Evaluator.MonkeyValue (
  isTruthy,
  unwrap,
  wrapLitPure,
) where

import Evaluator.Type (MonkeyValue (..), MonkeyValueObj (..))

isTruthy :: MonkeyValueObj -> Bool
isTruthy MonkeyNull = False
isTruthy (MonkeyBool b) = b
isTruthy _ = True

unwrap :: MonkeyValue -> MonkeyValueObj
unwrap (ReturnValue v) = v
unwrap (LiteralValue v) = v

wrapLitPure :: (Applicative m) => MonkeyValueObj -> m MonkeyValue
wrapLitPure = pure . LiteralValue