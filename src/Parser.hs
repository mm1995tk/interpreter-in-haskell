module Parser (Parser, sc, parseNumber, parseNull, parseBool, parseSymbol, parseLetStmt) where

import qualified AST
import Control.Monad (void)

-- import Data.Functor (($>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, some, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | space consumer
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

parseNumber :: Parser AST.Expr
parseNumber = AST.Number <$> L.decimal

parseBool :: Parser AST.Expr
parseBool = AST.Bool True <$ string "true" <|> AST.Bool False <$ string "false"

parseNull :: Parser AST.Expr
parseNull = AST.Null <$ string "null" <* sc

parseSymbol :: Parser AST.Expr
parseSymbol = AST.SymbolExpr . AST.Symbol . pack <$> (some alphaNumChar :: Parser String) <* sc

parseSemicolon :: Parser ()
parseSemicolon = void $ char ';' <* sc

parseLetStmt :: Parser AST.Statement
parseLetStmt = do
  AST.SymbolExpr symbol <- letKeyword *> (parseSymbol <?> "変数名") <* eqKeyword
  AST.Let symbol <$> parseExpr <* parseSemicolon
 where
  letKeyword = void $ string "let" <* sc
  eqKeyword = void $ char '=' <* sc

parseExpr :: Parser AST.Expr
parseExpr =
  choice
    [ parseNumber
    , parseBool
    , parseSymbol
    ]