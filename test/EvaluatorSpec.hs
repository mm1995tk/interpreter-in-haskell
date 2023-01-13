{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EvaluatorSpec where

import qualified Data.Text as T
import Evaluator (eval)
import Evaluator.Env (empty)
import Parser (parse, parseProgram)
import Support.TypeClass (displayText)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.Megaparsec (errorBundlePretty)

spec_eval :: Spec
spec_eval = do
  describe "eval" $ do
    it "fibonacci" $ createTest "test/resource/fibonacci.monkey" "5"
    it "higher_order_function" $ createTest "test/resource/higher_order_function.monkey" "9"

createTest :: String -> T.Text -> IO ()
createTest filePath expected = do
  input <- T.pack <$> readFile filePath
  case parse parseProgram input of
    Right v -> case displayText . fst <$> eval v empty of
      Right t -> t `shouldBe` expected
      Left err -> expectationFailure $ show err
    Left err -> expectationFailure $ errorBundlePretty err
