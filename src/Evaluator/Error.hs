{-# LANGUAGE ConstraintKinds #-}

module Evaluator.Error (EvalError(..), EvalErrorOr) where


data EvalError
  = NotImpl
  deriving (Show)

type EvalErrorOr a = Either EvalError a

