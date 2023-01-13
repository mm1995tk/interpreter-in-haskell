{-# OPTIONS_GHC -Wno-partial-fields #-}

module Evaluator.Type (
  MonkeyValue (..),
  MonkeyValueObj (..),
  Env (..),
  Evaluator,
  EvalError (..),
  EvalErrorOr,
  runEvaluator,
  throwErr,
  get,
  put,
) where

import AST (Identifier, Program)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import Support.TypeClass (Display (displayText))
import Text.Printf (printf)
import Prelude hiding (lookup)

type Evaluator output = StateT Env EvalErrorOr output

runEvaluator :: Evaluator output -> Env -> EvalErrorOr (output, Env)
runEvaluator = runStateT

throwErr :: EvalError -> Evaluator a
throwErr = lift . Left

data EvalError
  = NotImpl
  | Debug String
  deriving (Show, Eq)

type EvalErrorOr = Either EvalError

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

instance Display MonkeyValueObj where
  displayText (MonkeyInt n) = T.pack $ show n
  displayText (MonkeyBool b) = if b then "true" else "false"
  displayText (MonkeyFn{}) = "[function]"
  displayText MonkeyNull = "null"

newtype Env = Env (M.Map Text MonkeyValue)

instance Semigroup Env where
  (Env a) <> (Env b) = Env $ M.union b a

instance Show Env where
  show (Env e) = let list = M.toList e in f list
    where
      f :: [(Text, MonkeyValue)] -> String
      f [] = ""
      f ((k, v) : xs) = printf "%s: %s\n" (T.unpack k) (show v) ++ f xs

instance Eq Env where
  (Env a) == (Env b) = show a == show b