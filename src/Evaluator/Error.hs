{-# LANGUAGE ConstraintKinds #-}

module Evaluator.Error (EvalError (..), EvalErrorOr, throwErr) where

import Control.Monad.Trans.Class (MonadTrans (lift))

data EvalError
  = NotImpl
  | Debug String
  deriving (Show)

type EvalErrorOr = Either EvalError

throwErr :: (MonadTrans t) => EvalError -> t EvalErrorOr a
throwErr = lift . Left
