module Main (main) where
import Text.Megaparsec (parseTest)
import Parser (parseExpr)

main :: IO ()
main = do
  parseTest parseExpr "123 "
  parseTest parseExpr "null "
  parseTest parseExpr "abc "
  parseTest parseExpr "true "
  parseTest parseExpr "trueman "
  parseTest parseExpr "if (1) {} else {} "
