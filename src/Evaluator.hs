module Evaluator (
  eval,
  evalProgram,
) where

import AST (Expr (..), Identifier (..), InfixOp (..), Literal (..), PrefixOp (..), Program, Statement (..))
import qualified Evaluator.Env as EE
import Evaluator.Error (EvalError (..), EvalErrorOr, throwErr)
import qualified Evaluator.MonkeyValue as Monkey
import Evaluator.Type (Env, Evaluator, MonkeyValue (..), MonkeyValueObj (..), runEvaluator)
import qualified Evaluator.Type as Evaluator (get, put)

eval :: Program -> Env -> EvalErrorOr (MonkeyValueObj, Env)
eval p = runEvaluator (Monkey.unwrap <$> evalProgram p)

evalProgram :: Program -> Evaluator MonkeyValue
evalProgram [] = Monkey.wrapLitPure MonkeyNull
evalProgram p = last <$> mapM evalStmt p

evalStmt :: Statement -> Evaluator MonkeyValue
evalStmt (ExprStmt _ True) = Monkey.wrapLitPure MonkeyNull
evalStmt (ExprStmt e _) = evalExpr e
evalStmt (Return e) =
  evalExpr e >>= \case
    v@(ReturnValue _) -> pure v
    LiteralValue v -> pure . ReturnValue $ v
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
        _ -> throwErr NotImpl
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
    Nothing -> throwErr . Debug . show $ env
    Just mv -> pure mv
evalExpr (InfixExpr{..}) = do
  l <- Monkey.unwrap <$> evalExpr leftExpr
  r <- Monkey.unwrap <$> evalExpr rightExpr
  case infixOp of
    Plus -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a + b)
      (MonkeyBool a, MonkeyBool b) -> Monkey.wrapLitPure $ MonkeyBool (a || b)
      _ -> throwErr NotImpl
    Minus -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a - b)
      _ -> throwErr NotImpl
    Multiply -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a * b)
      (MonkeyBool a, MonkeyBool b) -> Monkey.wrapLitPure $ MonkeyBool (a && b)
      _ -> throwErr NotImpl
    Divide -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyInt (a `div` b)
      _ -> throwErr NotImpl
    Lt -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyBool (a < b)
      _ -> throwErr NotImpl
    Gt -> case (l, r) of
      (MonkeyInt a, MonkeyInt b) -> Monkey.wrapLitPure $ MonkeyBool (a > b)
      _ -> throwErr NotImpl
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
  localEnv <- Evaluator.get
  let program = body
  Monkey.wrapLitPure MonkeyFn{..}
evalExpr (CallExpr{..}) = do
  env <- Evaluator.get
  expr <- Monkey.unwrap <$> evalExpr called
  case expr of
    MonkeyFn{..} -> do
      evaluatedArgs <- mapM evalExpr args
      let keys = fmap (\(Identifier t) -> t) params
          pairs = zip keys evaluatedArgs

      env' <-
        if length keys == length pairs
          then pure $ EE.union localEnv (EE.union (EE.fromList pairs) env)
          else throwErr NotImpl

      Evaluator.put env'
      (value, _) <- case runEvaluator (evalProgram program) env' of
        Left err -> throwErr err
        Right v -> pure v
      value <$ Evaluator.put env
    _ -> throwErr NotImpl
