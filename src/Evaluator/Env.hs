module Evaluator.Env (
  empty,
  fromList,
  lookup,
  upsert,
  compose,
) where

import qualified Data.Map as M
import Data.Text (Text)
import Evaluator.Type (Env (..), MonkeyValue)

import Prelude hiding (lookup)

empty :: Env
empty = Env M.empty

fromList :: [(Text, MonkeyValue)] -> Env
fromList = Env . M.fromList

lookup :: Text -> Env -> Maybe MonkeyValue
lookup t (Env e) = M.lookup t e

upsert :: Text -> MonkeyValue -> Env -> Env
upsert t mv (Env e) = Env $ M.insert t mv e

compose :: Env -> [Env] -> Env
compose e [] = e
compose e [x] = e <> x
compose e (x : xs) = e <> compose x xs