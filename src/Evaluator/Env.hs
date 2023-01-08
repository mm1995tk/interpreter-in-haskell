module Evaluator.Env (
  empty,
  fromList,
  lookup,
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