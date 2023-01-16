{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parser where

import qualified AST
import qualified AST as ASt
import Control.Monad (void)
import Data.Functor (($>))
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
  ident <- parseIdent <?> "変数名"
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
              try . M.choice $
                [ parseCall leftExpr
                , parseInfix precedence leftExpr
                ]
            parseFoldExprFromLeft leftExpr'
       in expr <|> pure leftExpr

parseAtomicExpr :: Parser AST.Expr
parseAtomicExpr =
  M.choice
    [ parsePrefixExpr
    , parseLiteral
    , parseArr
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
parseInfix precedence left = do
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

parseIdent :: Parser AST.Identifier
parseIdent = wrapByIdent <$> (checkStartFromChar *> exec)
  where
    checkStartFromChar = M.lookAhead Mc.letterChar
    exec = lexeme $ M.some Mc.alphaNumChar
    wrapByIdent = AST.Identifier . pack

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
parseCommaSeparatedExprs = M.many (parseExprDefault <* M.optional (char ','))

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