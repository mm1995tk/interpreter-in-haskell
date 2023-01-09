{-# OPTIONS_GHC -Wno-partial-fields #-}

module Evaluator.Type (
  MonkeyValue (..),
  MonkeyValueObj (..),
  Env (..),
  Evaluator,
  runEvaluator,
  get,
  put,
) where

import AST (Identifier, Program)
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Evaluator.Error (EvalErrorOr)
import Text.Printf (printf)
import Prelude hiding (lookup)

type Evaluator output = StateT Env EvalErrorOr output

runEvaluator :: Evaluator output -> Env -> EvalErrorOr (output, Env)
runEvaluator = runStateT

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