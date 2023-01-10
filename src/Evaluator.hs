module Evaluator (
  eval,
  evalProgram,
) where

import AST (Expr (..), Identifier (..), InfixOp (..), Literal (..), PrefixOp (..), Program, Statement (..))
import Control.Monad (join)
import qualified Evaluator.Env as EE
import qualified Evaluator.MonkeyValue as Monkey
import Evaluator.Type (Env, EvalError (..), EvalErrorOr, Evaluator, MonkeyValue (..), MonkeyValueObj (..))
import qualified Evaluator.Type as Evaluator (get, put, runEvaluator, throwErr)

eval :: Program -> Env -> EvalErrorOr (MonkeyValueObj, Env)
eval p = Evaluator.runEvaluator (Monkey.unwrap <$> evalProgram p)

evalProgram :: Program -> Evaluator MonkeyValue
evalProgram [] = Monkey.wrapLitPure MonkeyNull
evalProgram [x] = evalStmt x
evalProgram (x : xs) =
  evalStmt x >>= \case
    ReturnValue v -> pure $ LiteralValue v
    _ -> evalProgram xs

evalStmt :: Statement -> Evaluator MonkeyValue
evalStmt (ExprStmt _ True) = Monkey.wrapLitPure MonkeyNull
evalStmt (ExprStmt e _) = evalExpr e
evalStmt (Return e) =
  evalExpr e >>= \case
    v@(ReturnValue _) -> pure v
    LiteralValue v -> pure $ ReturnValue v
evalStmt (Let (Identifier key) expr) = do
  env <- Evaluator.get
  evaluated <- evalExpr expr
  Evaluator.put $ EE.upsert key evaluated env
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
        _ -> Evaluator.throwErr NotImpl
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
  env <- Evaluator.get
  case EE.lookup ident env of
    Nothing -> Evaluator.throwErr . Debug . show $ env
    Just mv -> pure mv
evalExpr (InfixExpr{..}) = do
  l <- Monkey.unwrap <$> evalExpr leftExpr
  r <- Monkey.unwrap <$> evalExpr rightExpr
  case infixOp of
    Plus -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a + b)
      (MonkeyBool a, MonkeyBool b) -> Monkey.wrapLitPure $ MonkeyBool (a || b)
      _ -> Evaluator.throwErr NotImpl
    Minus -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a - b)
      _ -> Evaluator.throwErr NotImpl
    Multiply -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a * b)
      (MonkeyBool a, MonkeyBool b) -> Monkey.wrapLitPure $ MonkeyBool (a && b)
      _ -> Evaluator.throwErr NotImpl
    Divide -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a `div` b)
      _ -> Evaluator.throwErr NotImpl
    Lt -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyBool (a < b)
      _ -> Evaluator.throwErr NotImpl
    Gt -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyBool (a > b)
      _ -> Evaluator.throwErr NotImpl
    Eq -> Monkey.wrapLitPure $ MonkeyBool (l == r)
    NotEq -> Monkey.wrapLitPure $ MonkeyBool (l /= r)
evalExpr (IfExpr{..}) = do
  condition <- Monkey.unwrap <$> evalExpr cond
  if Monkey.isTruthy condition
    then evalProgram consequence
    else case alter of
      Just p -> evalProgram p
      _ -> pure $ LiteralValue MonkeyNull
evalExpr (FnExpr{body = program, ..}) = do
  localEnv <- Evaluator.get
  Monkey.wrapLitPure MonkeyFn{..}
evalExpr (CallExpr{..}) = join $ evalCallFn <$> Evaluator.get <*> (Monkey.unwrap <$> evalExpr called)
  where
    evalCallFn env MonkeyFn{..} = do
      evaluatedArgs <- mapM evalExpr args

      if length evaluatedArgs == length params
        then pure ()
        else Evaluator.throwErr NotImpl

      let keys = fmap (\(Identifier t) -> t) params
          pairs = zip keys evaluatedArgs
          env' = EE.compose env [localEnv, EE.fromList pairs]

      Evaluator.put env' *> evalProgram program <* Evaluator.put env
    evalCallFn _ _ = Evaluator.throwErr NotImpl
