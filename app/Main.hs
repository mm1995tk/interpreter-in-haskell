module Main (main) where

import Control.Concurrent
import Data.Functor (void, ($>))
import qualified Data.Text as T
import Evaluator (eval)
import qualified Evaluator.Env as EE
import Parser (parse, parseProgram)
import System.IO (hFlush, stdout)
import System.Posix.Signals (
  Handler (Catch),
  installHandler,
  keyboardSignal,
 )

main :: IO ()
main = do
  putStrLn "start..."
  repl

repl :: IO ()
repl = do
  setSignal
  putStrLn "Welcome to Monkey Language REPL.\n"
  loop EE.empty
  where
    setSignal = do
      tid <- myThreadId
      void $ installHandler keyboardSignal (Catch $ handler tid) Nothing

    handler tid = do
      putStrLn " goodbye!"
      killThread tid

    loop e = do
      putStr ">> "
      hFlush stdout
      input <- T.pack <$> getLine
      e' <- case parse parseProgram input of
        Left err -> print err $> e
        Right p -> case eval p e of
          Left err -> print err $> e
          Right (v, e') -> print v $> e'
      putStrLn ""
      loop e'
