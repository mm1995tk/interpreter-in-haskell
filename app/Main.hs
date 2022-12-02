module Main (main) where

import Data.Text (pack)
import Parser (parseBool, parseLetStmt, parseNull, parseNumber)
import Text.Megaparsec (parseTest)

main :: IO ()
main = do
  -- let a = parse parseNumber ""  (pack "123")
  parseTest parseNumber (pack "12a3")
  parseTest parseNull (pack "null")
  parseTest parseBool (pack "true")
  parseTest parseBool (pack "false")
  parseTest parseLetStmt (pack "let c = 3;")
  parseTest parseLetStmt (pack "let c = true;")
  parseTest parseLetStmt (pack "let c = a;")
  parseTest parseLetStmt (pack "let = a;")
