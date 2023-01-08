module Evaluator (
  eval,
  evalProgram,
) where

import AST (Expr (..), Identifier (..), InfixOp (..), Literal (..), PrefixOp (..), Program, Statement (..))
import qualified Evaluator.Combinator as EC
import qualified Evaluator.Env as EE
import Evaluator.Error (EvalError (NotImpl), EvalErrorOr)
import qualified Evaluator.MonkeyValue as Monkey
import Evaluator.Type (Env, Evaluator (..), MonkeyValue (..), MonkeyValueObj (..))

eval :: Program -> Env -> EvalErrorOr (MonkeyValueObj, Env)
eval p = runEvaluator (Monkey.unwrap <$> evalProgram p)

evalProgram :: Program -> Evaluator MonkeyValue
evalProgram [] = Monkey.wrapLitPure MonkeyNull
evalProgram (x : _) = evalStmt x

evalStmt :: Statement -> Evaluator MonkeyValue
evalStmt (ExprStmt _ True) = Monkey.wrapLitPure MonkeyNull
evalStmt (ExprStmt e _) = evalExpr e
evalStmt (Return e) =
  evalExpr e >>= \case
    v@(ReturnValue _) -> pure v
    LiteralValue v -> pure . ReturnValue $ v
evalStmt (Let (Identifier key) expr) = do
  env <- EC.get
  evaluated <- evalExpr expr
  EC.put $ EE.upsert key evaluated env
  Monkey.wrapLitPure MonkeyNull

evalExpr :: Expr -> Evaluator MonkeyValue
evalExpr (LiteralExpr l) = Monkey.wrapLitPure $ case l of
  NumLiteral n -> MonkeyInt n
  BoolLiteral b -> MonkeyBool b
  Null -> MonkeyNull
evalExpr (PrefixExpr op expr) = case op of
  MinusPrefix ->
    evaluated >>= \case
      v@(ReturnValue _) -> pure v
      LiteralValue v -> case v of
        MonkeyInt n -> Monkey.wrapLitPure $ MonkeyInt (-n)
        _ -> EC.throwErr NotImpl
  Not ->
    evaluated >>= \case
      v@(ReturnValue _) -> pure v
      LiteralValue v -> case v of
        MonkeyInt _ -> Monkey.wrapLitPure $ MonkeyBool False
        MonkeyBool b -> Monkey.wrapLitPure $ MonkeyBool (not b)
        MonkeyNull -> Monkey.wrapLitPure $ MonkeyBool True
        MonkeyFn{} -> Monkey.wrapLitPure $ MonkeyBool False
  where
    evaluated = evalExpr expr
evalExpr (IdentExpr (Identifier ident)) = do
  maybeValue <- EE.lookup ident <$> EC.get
  case maybeValue of
    Nothing -> EC.throwErr NotImpl
    Just mv -> pure mv
evalExpr (InfixExpr{..}) = do
  l <- Monkey.unwrap <$> evalExpr leftExpr
  r <- Monkey.unwrap <$> evalExpr rightExpr
  case infixOp of
    Plus -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a + b)
      (MonkeyBool a, MonkeyBool b) -> Monkey.wrapLitPure $ MonkeyBool (a || b)
      _ -> EC.throwErr NotImpl
    Minus -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a - b)
      _ -> EC.throwErr NotImpl
    Multiply -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a * b)
      (MonkeyBool a, MonkeyBool b) -> Monkey.wrapLitPure $ MonkeyBool (a && b)
      _ -> EC.throwErr NotImpl
    Divide -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a `div` b)
      _ -> EC.throwErr NotImpl
    Lt -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyBool (a < b)
      _ -> EC.throwErr NotImpl
    Gt -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyBool (a > b)
      _ -> EC.throwErr NotImpl
    Eq -> Monkey.wrapLitPure $ MonkeyBool (l == r)
    NotEq -> Monkey.wrapLitPure $ MonkeyBool (l /= r)
evalExpr (IfExpr{..}) = do
  condition <- Monkey.unwrap <$> evalExpr cond
  if Monkey.isTruthy condition
    then evalProgram consequence
    else case alter of
      Just p -> evalProgram p
      _ -> pure $ LiteralValue MonkeyNull
evalExpr (FnExpr{..}) = do
  localEnv <- EC.get
  let program = body
  Monkey.wrapLitPure MonkeyFn{..}
evalExpr _ = undefined
