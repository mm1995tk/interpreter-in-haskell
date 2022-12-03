module Parser (Parser, parseExpr) where

import qualified AST
import Control.Monad (void)
import Data.Functor ()
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, try, (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as Mc
import qualified Text.Megaparsec.Char.Lexer as Mcl

type Parser = Parsec Void Text

-- | スペースとコメントをスキップ
sc :: Parser ()
sc =
  Mcl.space
    Mc.space1
    (Mcl.skipLineComment "//")
    (Mcl.skipBlockComment "/*" "*/")

-- | 'sc'を末尾に適用
lexeme :: Parser a -> Parser a
lexeme = Mcl.lexeme sc

-- |文字
char :: Char -> Parser ()
char = void . lexeme . Mc.char

-- |予約語
keyword :: Text -> Parser ()
keyword t = void . lexeme . try $ Mc.string t <* M.notFollowedBy Mc.alphaNumChar

betweenParen :: Parser a -> Parser a
betweenParen = lexeme . M.between "(" ")"

betweenBrace :: Parser a -> Parser a
betweenBrace = lexeme . M.between "{" "}"

semicolon :: Parser ()
semicolon = char ';'

parseLiteral :: Parser AST.Literal
parseLiteral = M.choice [parseNumber, parseBool, parseNull]

parseNumber :: Parser AST.Literal
parseNumber = AST.NumLiteral <$> lexeme Mcl.decimal

parseBool :: Parser AST.Literal
parseBool =
  pt <|> pf
 where
  pt = AST.BoolLiteral True <$ keyword "true"
  pf = AST.BoolLiteral False <$ keyword "false"

parseNull :: Parser AST.Literal
parseNull = AST.Null <$ keyword "null"

parseIdent :: Parser AST.Identifier
parseIdent = AST.Identifier . pack <$> lexeme (M.some Mc.alphaNumChar :: Parser String)

parseExpr :: Parser AST.Expr
parseExpr =
  M.choice
    [ AST.mapToExpr parseLiteral
    , parseIfExpr
    , AST.mapToExpr parseIdent
    ]

parseIfExpr :: Parser AST.Expr
parseIfExpr = do
  keyword "if"
  cond <- betweenParen parseExpr
  consequence <- betweenBrace $ pure [] :: Parser AST.Program
  alter <- M.optional $ do
    keyword "else"
    betweenBrace $ pure [] :: Parser AST.Program
  return AST.IfExpr{..}

-- parseSymbol :: Parser AST.Expr
-- parseSymbol = AST.SymbolExpr . AST.Symbol . pack <$> lexeme (some alphaNumChar :: Parser String)

-- parseLetStmt :: Parser AST.Statement
-- parseLetStmt = do
--   expr <- letKeyword *> (parseExpr <?> "変数名") <* eqKeyword
--   symbol <- case expr of
--     (AST.SymbolExpr symbol) -> pure symbol
--     _ -> throwErr Panic
--   AST.Let symbol <$> parseExpr <* parseSemicolon
--  where
--   letKeyword = void $ keyword "let"
--   eqKeyword = parseCode '='

-- parseExpr :: Parser AST.Expr
-- parseExpr =
--   choice
--     [ parseNumber
--     , try parseBool
--     , try parseNull
--     , parseSymbol
--     ]

-- |パーサエラー
-- data ParserErr
--   = Panic
--   | UnexpectedToken Text Text

-- instance Show ParserErr where
--   show = \case
--     Panic -> "panic!"
--     UnexpectedToken expected obtained ->
--       "expected: " ++ unpack expected ++ "obtained: " ++ unpack obtained

-- throwErr :: ParserErr -> Parser a
-- throwErr = fail . show
