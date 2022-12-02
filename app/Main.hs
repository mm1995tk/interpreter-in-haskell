module Main (main) where

import Lib (plusOne)
import Parser (parseNumber, parseNull, parseBool)
import Text.Megaparsec (parse, parseTest)
import Data.Text (pack)

main :: IO ()
main = do
  -- let a = parse parseNumber ""  (pack "123")
  parseTest parseNumber (pack "12a3")
  parseTest parseNull (pack "null")
  parseTest parseBool (pack "true")
  parseTest parseBool (pack "false")
  print  0
