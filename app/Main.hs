module Main (main) where
import Text.Megaparsec (parseTest)
import Parser (parseExpr, parseStmt)

main :: IO ()
main = do
  parseTest parseExpr "123 "
  parseTest parseExpr "null "
  parseTest parseExpr "abc "
  parseTest parseExpr "true "
  parseTest parseExpr "trueman "
  parseTest parseExpr "if (1) {} else {} "
  parseTest parseStmt "let c1 = null;"
  parseTest parseStmt "let null = null;"
  parseTest parseStmt "let nullable = null;"
