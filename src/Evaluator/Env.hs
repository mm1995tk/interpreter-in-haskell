module Evaluator.Env (Env, empty, fromList, lookup) where

import qualified Data.Map as M
import Data.Text (Text)
import Evaluator.MonkeyValue (MonkeyValue)
import Prelude hiding (lookup)

type Env = M.Map Text MonkeyValue

empty :: Env
empty = M.empty

fromList :: [(Text, MonkeyValue)] -> Env
fromList = M.fromList

lookup :: Text -> Env -> Maybe MonkeyValue
lookup = M.lookup