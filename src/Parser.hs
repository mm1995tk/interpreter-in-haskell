module Parser (Parser, sc, parseNumber, parseNull,parseBool) where

import AST (Expr (..))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (oneOf)
import Text.Megaparsec.Char (space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | space consumer
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

parseNumber :: Parser Expr
parseNumber = Number <$> L.decimal

parseBool :: Parser Expr
parseBool = Bool True <$ string "true" <|> Bool False <$ string "false"

parseNull :: Parser Expr
parseNull = Null <$ string "null"
