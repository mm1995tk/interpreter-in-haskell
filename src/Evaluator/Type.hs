{-# OPTIONS_GHC -Wno-partial-fields #-}

module Evaluator.Type (
  MonkeyValue (..),
  MonkeyValueObj (..),
  Env (..),
  Evaluator (..),
) where

import AST (Identifier, Program)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Evaluator.Error (EvalErrorOr)
import Text.Printf (printf)
import Prelude hiding (lookup)

newtype Evaluator output = Evaluator {runEvaluator :: Env -> EvalErrorOr (output, Env)} deriving (Functor)

instance Applicative Evaluator where
  pure a = Evaluator $ \env -> Right (a, env)

  Evaluator f <*> Evaluator a = Evaluator $ \env -> do
    (aa, env') <- a env
    (ff, env'') <- f env'
    return (ff aa, env'')

instance Monad Evaluator where
  Evaluator a >>= f = Evaluator $ \env -> do
    (a', env') <- a env
    let Evaluator b = f a'
    b env'

data MonkeyValue
  = ReturnValue MonkeyValueObj
  | LiteralValue MonkeyValueObj
  deriving (Show, Eq)

data MonkeyValueObj
  = MonkeyInt Int
  | MonkeyBool Bool
  | MonkeyFn {params :: [Identifier], program :: Program, localEnv :: Env}
  | MonkeyNull
  deriving (Show, Eq)

newtype Env = Env (M.Map Text MonkeyValue)

instance Show Env where
  show (Env e) = let list = M.toList e in f list
    where
      f :: [(Text, MonkeyValue)] -> String
      f [] = ""
      f ((k, v) : xs) = printf "%s: %s\n" (T.unpack k) (show v) ++ f xs

instance Eq Env where
  (Env a) == (Env b) = show a == show b