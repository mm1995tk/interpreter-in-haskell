module Parser (Parser, sc, parseNumber, parseNull, parseBool, parseSymbol) where

import qualified AST
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, space1, string)
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
parseNull = AST.Null <$ string "null"

parseSymbol :: Parser AST.Expr
parseSymbol = AST.SymbolExpr . AST.Symbol . pack <$> (some alphaNumChar :: Parser String)
