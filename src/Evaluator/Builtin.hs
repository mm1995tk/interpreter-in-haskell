module Evaluator.Builtin (builtinFns) where

import qualified Data.Map as M
import qualified Data.Text as T
import Evaluator.Type
import Prelude hiding (head, tail)
import qualified Prelude

builtinFns :: M.Map T.Text MonkeyValue
builtinFns =
  M.fromList . toTupleList $
    [ len
    , push
    , head
    , tail
    ]
  where
    toTupleList :: [BuiltinFn] -> [(T.Text, MonkeyValue)]
    toTupleList = fmap $ \item@BuiltinFn{name} -> (name, LiteralValue $ MonkeyBuiltinFn item)

len :: BuiltinFn
len = BuiltinFn{..}
  where
    name = "len"
    cntOfParams = 1
    fn [LiteralValue (MonkeyArr arr)] = Right . LiteralValue . MonkeyInt . length $ arr
    fn [LiteralValue (MonkeyStr str)] = Right . LiteralValue . MonkeyInt . length . T.unpack $ str
    fn _ = Left $ NotImpl

head :: BuiltinFn
head = BuiltinFn{..}
  where
    name = "head"
    cntOfParams = 1
    fn [LiteralValue (MonkeyArr arr@(_ : _))] = Right . LiteralValue . Prelude.head $ arr
    fn _ = Left $ NotImpl

tail :: BuiltinFn
tail = BuiltinFn{..}
  where
    name = "tail"
    cntOfParams = 1
    fn [LiteralValue (MonkeyArr arr@(_ : _))] = Right . LiteralValue . MonkeyArr . Prelude.tail $ arr
    fn _ = Left $ NotImpl

push :: BuiltinFn
push = BuiltinFn{..}
  where
    name = "push"
    cntOfParams = 2
    fn [LiteralValue (MonkeyArr arr), LiteralValue a] = Right . LiteralValue . MonkeyArr . reverse $ (a : reverse arr)
    fn _ = Left $ NotImpl
