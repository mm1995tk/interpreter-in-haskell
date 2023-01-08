module Evaluator (
  eval,evalProgram
) where

import AST (Expr (..), Literal (..), PrefixOp (..), Program, Statement (..))
import qualified Evaluator.Combinator as E
import Evaluator.Error (EvalError (NotImpl), EvalErrorOr)
import Evaluator.MonkeyValue (MonkeyValueObj (..))

eval :: Program -> EvalErrorOr MonkeyValueObj
eval p = E.evalOutput (evalProgram p) undefined

evalProgram :: Program -> E.Evaluator MonkeyValueObj
evalProgram [] = pure MonkeyNull
evalProgram (x : _) = evalStmt x

evalStmt :: Statement -> E.Evaluator MonkeyValueObj
evalStmt (ExprStmt _ True) = pure MonkeyNull
evalStmt (ExprStmt e _) = evalExpr e
evalStmt (Return _) = undefined
evalStmt (Let{}) = undefined

evalExpr :: Expr -> E.Evaluator MonkeyValueObj
evalExpr (LiteralExpr l) = pure $ case l of
  NumLiteral n -> MonkeyInt n
  BoolLiteral b -> MonkeyBool b
  Null -> MonkeyNull
evalExpr (PrefixExpr op expr) = case op of
  MinusPrefix ->
    k >>= \case
      MonkeyInt n -> pure $ MonkeyInt (-n)
      _ -> E.throwErr NotImpl
  Not ->
    k >>= \case
      MonkeyInt _ -> E.throwErr NotImpl
      MonkeyBool b -> pure $ MonkeyBool (not b)
      MonkeyNull -> pure $ MonkeyBool True
  where
    k = evalExpr expr
evalExpr _ = undefined
