{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parser where

import qualified AST
import qualified AST as ASt
import Control.Monad (void)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
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
parse = flip M.parse ""

parseTest :: Text -> IO ()
parseTest = M.parseTest parseProgram

parseProgram :: Parser AST.Program
parseProgram = sc *> M.many parseStmt <* M.eof

parseStmt :: Parser AST.Statement
parseStmt = M.choice [parseLetStmt, parseReturnStmt, parseExprStmt]

parseBlockStmt :: Parser ASt.Program
parseBlockStmt = betweenBrace $ M.many parseStmt

parseLetStmt :: Parser AST.Statement
parseLetStmt = do
  keyword "let"
  ident <- parseIdent
  char '='
  expr <- parseExprDefault
  semicolon
  return AST.Let{..}

parseReturnStmt :: Parser AST.Statement
parseReturnStmt = AST.Return <$ keyword "return" <*> parseExprDefault <* semicolon

parseExprStmt :: Parser AST.Statement
parseExprStmt = AST.ExprStmt <$> parseExprDefault <*> (isJust <$> M.optional semicolon)

parseExprDefault :: Parser AST.Expr
parseExprDefault = parseExpr AST.Lowest

parseExpr :: AST.PrecedenceOfInfixOp -> Parser AST.Expr
parseExpr precedence = do
  leftExpr <- M.choice [try parseGroupExpr, parseAtomicExpr]
  maybeExpr <- M.optional (parseFoldExprFromLeft leftExpr)
  return $ fromMaybe leftExpr maybeExpr
  where
    parseGroupExpr = betweenParen parseExprDefault
    parseFoldExprFromLeft leftExpr =
      let expr = do
            leftExpr' <-
              M.choice
                [ parseCall leftExpr
                , parseInfix precedence leftExpr
                , parseAccessExpr leftExpr
                ]
            parseFoldExprFromLeft leftExpr'
       in expr <|> pure leftExpr

parseAtomicExpr :: Parser AST.Expr
parseAtomicExpr =
  M.choice
    [ parsePrefixExpr
    , parseLiteral
    , parseArr
    , parseHash
    , parseIfExpr
    , parseFn
    , fmap AST.IdentExpr parseIdent
    ]
    <?> "expression"

parsePrefixExpr :: Parser AST.Expr
parsePrefixExpr = AST.PrefixExpr <$> parsePrefixOp <*> parseExprDefault
  where
    parsePrefixOp = M.choice . fmap lexToken $ [AST.MinusPrefix, AST.Not]

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

parseInfix :: AST.PrecedenceOfInfixOp -> AST.Expr -> Parser AST.Expr
parseInfix precedence left = try $ do
  infixOp <- parseInfixOp
  let precedence' = AST.getInfixPrecedence infixOp
  if precedence' > precedence
    then AST.InfixExpr infixOp left <$> parseExpr precedence'
    else ParserError.throwError $ ParserError.UnexpectedToken "" (displayText infixOp)
  where
    parseInfixOp =
      M.choice . fmap lexToken $
        [ AST.Plus
        , AST.Minus
        , AST.Multiply
        , AST.Divide
        , AST.Mod
        , AST.Lt
        , AST.Gt
        , AST.Eq
        , AST.NotEq
        ]

parseCall :: AST.Expr -> Parser AST.Expr
parseCall fn =
  AST.CallExpr
    <$> callExpr fn
    <*> betweenParen parseCommaSeparatedExprs
  where
    callExpr = \case
      AST.LiteralExpr _ -> ParserError.throwError $ ParserError.UnexpectedToken "expression that returns a function when evaluated" "literal"
      called' -> return called'

parseArr :: Parser AST.Expr
parseArr = AST.ArrExpr <$> betweenBracket parseCommaSeparatedExprs

parseHash :: Parser AST.Expr
parseHash = AST.HashMapExpr <$> betweenBrace parseKeyValues
  where
    parseKeyValues = Map.fromList <$> parseCommaSeparated parseKeyValue
    parseKeyValue = (,) <$> parseExprDefault <*> (char ':' *> parseExprDefault)

parseAccessExpr :: AST.Expr -> Parser AST.Expr
parseAccessExpr target = do accessor <- betweenBracket parseExprDefault; pure $ AST.AccessExpr{..}

parseIdent :: Parser AST.Identifier
parseIdent = wrapByIdent <$> (eliminateKeyword *> checkStartFromChar *> exec)
  where
    checkStartFromChar = M.lookAhead Mc.letterChar <?> "文字はじまりの文字列"
    exec = lexeme $ M.some Mc.alphaNumChar
    wrapByIdent = AST.Identifier . pack
    eliminateKeyword = M.notFollowedBy (M.choice . fmap keyword $ ["let", "return", "true", "false"]) <?> "予約語でない文字はじまりの文字列"

parseLiteral :: Parser AST.Expr
parseLiteral = AST.LiteralExpr <$> (M.choice [parseNumber, parseBool, parseNull, parseStr] <?> "literals")
  where
    parseStr =
      let exec = lexeme $ M.some Mc.alphaNumChar
          between = lexeme . M.between (char '"') (char '"')
       in AST.StrLiteral . pack <$> between exec
    parseNull = AST.Null <$ keyword "null"
    parseNumber = AST.NumLiteral <$> lexeme (Mcl.decimal <* M.notFollowedBy Mc.alphaNumChar)
    parseBool =
      (AST.BoolLiteral True <$ keyword "true")
        <|> (AST.BoolLiteral False <$ keyword "false")

parseCommaSeparatedExprs :: Parser [AST.Expr]
parseCommaSeparatedExprs = parseCommaSeparated parseExprDefault

parseCommaSeparated :: Parser a -> Parser [a]
parseCommaSeparated p = try isTrailingComma <|> notTrailingComma
  where
    isTrailingComma = M.many (p <* char ',')
    notTrailingComma =
      M.optional p >>= \case
        Nothing -> return []
        Just expr -> (expr :) <$> M.many (char ',' *> p)

lexToken :: (Display a) => a -> Parser a
lexToken token = (lexeme . Mc.string . displayText $ token) $> token

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

betweenBracket :: Parser a -> Parser a
betweenBracket = lexeme . M.between (char '[') (char ']')

semicolon :: Parser ()
semicolon = char ';'