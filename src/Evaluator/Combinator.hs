module Evaluator.Combinator (
  Evaluator (..),
  evalOutput,
  execEvaluator,
  get,
  put,
  throwErr,
) where

import Evaluator.Env (Env)
import Evaluator.Error (EvalError, EvalErrorOr)

newtype Evaluator output = Evaluator {eval :: Env -> EvalErrorOr (output, Env)} deriving (Functor)

instance Applicative Evaluator where
  pure a = Evaluator $ \env -> Right (a, env)

  Evaluator f <*> Evaluator a = Evaluator $ \env -> do
    (aa, env') <- a env
    (ff, env'') <- f env'
    return (ff aa, env'')

instance Monad Evaluator where
  Evaluator a >>= f = Evaluator $ \env -> do
    (a', env') <- a env
    let Evaluator b = f a'
    b env'

evalOutput :: Evaluator b -> Env -> EvalErrorOr b
evalOutput evaluator e = fst <$> eval evaluator e

execEvaluator :: Evaluator a -> Env -> Either EvalError Env
execEvaluator evaluator e = snd <$> eval evaluator e

get :: Evaluator Env
get = Evaluator $ \e -> Right (e, e)

put :: Env -> Evaluator ()
put e = Evaluator $ \_ -> Right ((), e)

throwErr :: EvalError -> Evaluator a
throwErr e = Evaluator $ \_ -> Left e