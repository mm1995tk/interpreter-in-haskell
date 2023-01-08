module Evaluator.Combinator (
  evalOutput,
  execEvaluator,
  get,
  put,
  throwErr,
) where

import Evaluator.Error (EvalError, EvalErrorOr)
import Evaluator.Type (Env, Evaluator (..))

evalOutput :: Evaluator b -> Env -> EvalErrorOr b
evalOutput evaluator e = fst <$> runEvaluator evaluator e

execEvaluator :: Evaluator a -> Env -> Either EvalError Env
execEvaluator evaluator e = snd <$> runEvaluator evaluator e

get :: Evaluator Env
get = Evaluator $ \e -> Right (e, e)

put :: Env -> Evaluator ()
put e = Evaluator $ \_ -> Right ((), e)

throwErr :: EvalError -> Evaluator a
throwErr e = Evaluator $ \_ -> Left e