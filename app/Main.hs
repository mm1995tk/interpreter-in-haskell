module Main (main) where

import Parser (parseExprDefault)
import Text.Megaparsec (parseTest)

main :: IO ()
main = do
  -- parseTest parseExpr "123 "
  -- parseTest parseExpr "null "
  -- parseTest parseExpr "abc "
  -- parseTest parseExpr "true "
  -- parseTest parseExpr "trueman "
  -- parseTest parseExpr "if (1) {} else {} "
  -- parseTest parseStmt "let c1 = null;"
  -- parseTest parseStmt "let null = null;"
  -- parseTest parseStmt "let nullable = null;"
  -- parseTest parseExpr "fn( x , y   , ) {  let nullable = null;  let nullable = null; }"
  parseTest parseExprDefault "1  + != 2 + 3 * 4"
  parseTest parseExprDefault "1   + 2 + 3 * 4"
  parseTest parseExprDefault "1  - 2 + 3 * 4"
