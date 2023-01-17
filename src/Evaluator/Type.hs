{-# OPTIONS_GHC -Wno-partial-fields #-}

module Evaluator.Type (
  MonkeyValue (..),
  MonkeyValueObj (..),
  Env (..),
  Evaluator,
  EvalError (..),
  EvalErrorOr,
  BuiltinFn (..),
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
  deriving (Show, Eq, Ord)

data MonkeyValueObj
  = MonkeyInt Int
  | MonkeyStr Text
  | MonkeyBool Bool
  | MonkeyArr [MonkeyValueObj]
  | MonkeyHashMap (M.Map MonkeyValueObj MonkeyValueObj)
  | MonkeyFn {params :: [Identifier], program :: Program, localEnv :: Env}
  | MonkeyBuiltinFn BuiltinFn
  | MonkeyNull
  deriving (Show, Eq, Ord)

instance Display MonkeyValueObj where
  displayText (MonkeyInt n) = T.pack $ show n
  displayText (MonkeyStr str) = T.concat ["\"", str, "\""]
  displayText (MonkeyBool b) = if b then "true" else "false"
  displayText (MonkeyFn{}) = "[function]"
  displayText MonkeyNull = "null"
  displayText (MonkeyArr []) = "[]"
  displayText (MonkeyArr nonemptyArr) =
    let content = T.concat $ fmap (\expr -> T.concat [displayText expr, ","]) nonemptyArr
     in T.concat ["[", content, "]"]
  displayText (MonkeyHashMap m) = T.pack $ show m
  displayText (MonkeyBuiltinFn m) = T.pack $ show m

newtype Env = Env (M.Map Text MonkeyValue) deriving (Ord)

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

data BuiltinFn = BuiltinFn
  { name :: Text
  , cntOfParams :: Int
  , fn :: [MonkeyValue] -> EvalErrorOr MonkeyValue
  }

instance Show BuiltinFn where
  show (BuiltinFn{name}) = T.unpack name

instance Eq BuiltinFn where
  (BuiltinFn{name = aname, cntOfParams = acnt}) == (BuiltinFn{name = bname, cntOfParams = bcnt}) = aname == bname && acnt == bcnt

instance Ord BuiltinFn where
  a@(BuiltinFn{name = aname}) <= b@(BuiltinFn{name = bname}) = a == b || aname < bname
