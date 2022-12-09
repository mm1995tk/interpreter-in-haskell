{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parser where

import qualified AST
import qualified AST as ASt
import Control.Monad (void)
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Parser.Error (Error (..))

import qualified Parser.Error as ParserError
import Support.TypeClass (Display (..))
import Text.Megaparsec (Parsec, try, (<?>), (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as Mc
import qualified Text.Megaparsec.Char.Lexer as Mcl

type Parser = Parsec Error Text

parse :: Parser a -> Text -> Either (M.ParseErrorBundle Text Error) a
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
betweenParen = lexeme . M.between (char '(') (char ')')

betweenBrace :: Parser a -> Parser a
betweenBrace = lexeme . M.between (char '{') (char '}')

semicolon :: Parser ()
semicolon = char ';'

parseLiteral :: Parser AST.Literal
parseLiteral = M.choice [parseNumber, parseBool, parseNull]

parseNumber :: Parser AST.Literal
parseNumber = AST.NumLiteral <$> lexeme (Mcl.decimal <* M.notFollowedBy Mc.alphaNumChar)

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

parsePrefixExpr :: Parser AST.Expr
parsePrefixExpr = AST.PrefixExpr <$> parsePrefixOp <*> parseExprDefault
  where
    parsePrefixOp = M.choice . fmap lexToken $ [AST.MinusPrefix, AST.Not]

parseExprDefault :: Parser AST.Expr
parseExprDefault = parseExpr AST.Lowest

parseExpr :: AST.PrecedenceOfInfixOp -> Parser AST.Expr
parseExpr precedence = do
  leftExpr <- M.choice [try parseCall, parseAtomicExpr]
  M.optional parseInfixOp >>= \case
    Just infixOp
      | AST.getInfixPrecedence infixOp > precedence ->
        AST.InfixExpr infixOp leftExpr <$> (M.notFollowedBy parseInfixOp *> parseExpr precedence)
    _ -> return leftExpr
  where
    parseInfixOp =
      M.choice . fmap lexToken $
        [ AST.Plus
        , AST.Minus
        , AST.Multiply
        , AST.Divide
        , AST.Lt
        , AST.Gt
        , AST.Eq
        , AST.NotEq
        ]

parseAtomicExpr :: Parser AST.Expr
parseAtomicExpr =
  M.choice
    [ parsePrefixExpr
    , AST.mapToExpr parseLiteral
    , parseIfExpr
    , parseFn
    , AST.mapToExpr parseIdent
    ]
    <?> "expression"

parseIfExpr :: Parser AST.Expr
parseIfExpr =
  (keyword "if" $> AST.IfExpr)
    <*> betweenParen parseExprDefault
    <*> parseBlockStmt
    <*> M.optional (keyword "else" *> parseBlockStmt)

parseFn :: Parser AST.Expr
parseFn =
  (keyword "fn" $> AST.FnExpr)
    <*> betweenParen (M.many $ parseIdent <* M.optional (char ','))
    <*> parseBlockStmt

parseCall :: Parser AST.Expr
parseCall =
  AST.CallExpr
    <$> ( parseAtomicExpr >>= \case
            AST.LiteralExpr _ -> ParserError.throwError ParserError.Panic
            called' -> return called'
        )
    <*> betweenParen (M.many $ parseExprDefault <* M.optional (char ','))

parseStmt :: Parser AST.Statement
parseStmt = M.choice [parseLetStmt, parseReturnStmt, parseExprStmt]

parseBlockStmt :: Parser ASt.Program
parseBlockStmt = betweenBrace $ M.many parseStmt

parseLetStmt :: Parser AST.Statement
parseLetStmt = do
  ident <- letKeyword *> (parseIdent <?> "変数名") <* eqKeyword
  expr <- parseExprDefault <* semicolon
  return AST.Let{..}
  where
    letKeyword = keyword "let"
    eqKeyword = char '='

parseReturnStmt :: Parser AST.Statement
parseReturnStmt = do
  keyword "return"
  AST.Return <$> parseExprDefault <* semicolon

parseExprStmt :: Parser AST.Statement
parseExprStmt = do
  expr <- parseExprDefault
  isSemicolon <- isJust <$> M.optional semicolon
  return AST.ExprStmt{..}

lexToken :: (Display a) => a -> Parser a
lexToken token = (lexeme . Mc.string . displayText $ token) $> token
