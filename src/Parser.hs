{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parser where

import qualified AST
import Control.Monad (void)
import Data.Functor ()
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, try, (<?>), (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as Mc
import qualified Text.Megaparsec.Char.Lexer as Mcl

type Parser = Parsec Void Text

parse :: Parser a -> Text -> Either (M.ParseErrorBundle Text Void) a
parse p = M.parse p ""

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
parseNumber = AST.NumLiteral <$> lexeme (try $ Mcl.decimal <* M.notFollowedBy Mc.alphaNumChar)

parseBool :: Parser AST.Literal
parseBool =
  pt <|> pf
 where
  pt = AST.BoolLiteral True <$ keyword "true"
  pf = AST.BoolLiteral False <$ keyword "false"

parseNull :: Parser AST.Literal
parseNull = AST.Null <$ keyword "null"

parseIdent :: Parser AST.Identifier
parseIdent = wrapByIdent <$> (checkStartFromChar *> exec)
 where
  checkStartFromChar = M.lookAhead Mc.letterChar
  exec = lexeme $ M.some Mc.alphaNumChar
  wrapByIdent = AST.Identifier . pack

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

parseStmt :: Parser AST.Statement
parseStmt = M.choice [parseLetStmt, parseReturnStmt, parseExprStmt]

parseLetStmt :: Parser AST.Statement
parseLetStmt = do
  ident <- letKeyword *> (parseIdent <?> "変数名") <* eqKeyword
  expr <- parseExpr <* semicolon
  return AST.Let{..}
 where
  letKeyword = void $ keyword "let"
  eqKeyword = char '='

parseReturnStmt :: Parser AST.Statement
parseReturnStmt = do
  void $ keyword "return"
  AST.Return <$> parseExpr <* semicolon

parseExprStmt :: Parser AST.Statement
parseExprStmt = do
  expr <- parseExpr
  isSemicolon <- isJust <$> M.optional semicolon
  return AST.ExprStmt{..}

{- |パーサエラー
 data ParserErr
   = Panic
   | UnexpectedToken Text Text
-}

-- instance Show ParserErr where
--   show = \case
--     Panic -> "panic!"
--     UnexpectedToken expected obtained ->
--       "expected: " ++ unpack expected ++ "obtained: " ++ unpack obtained

-- throwErr :: ParserErr -> Parser a
-- throwErr = fail . show
