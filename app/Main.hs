module Main (main) where

import Control.Concurrent
import qualified Data.Text as T
import Evaluator (eval)
import Parser (parse, parseProgram)
import System.IO (hFlush, stdout)
import System.Posix.Signals (
  Handler (Catch),
  installHandler,
  keyboardSignal,
 )

main :: IO ()
main = do
  repl

repl :: IO ()
repl = do
  setSignal
  putStrLn "Welcome to Monkey Language REPL.\n"
  loop
  where
    setSignal = do
      tid <- myThreadId
      _ <- installHandler keyboardSignal (Catch $ handler tid) Nothing
      return ()
    handler tid = do
      putStrLn "goodbye!"
      killThread tid
    loop = do
      putStr ">> "
      hFlush stdout
      input <- T.pack <$> getLine
      putStrLn $ case parse parseProgram input of
        Left e -> show e
        Right p -> show $ eval p
      putStrLn ""
      loop
