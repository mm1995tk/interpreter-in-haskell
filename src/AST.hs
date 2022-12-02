{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module AST (
  Expr (..),
  Program,
  Statement (..),
  Symbol (..),
  InfixOp (..),
  PrefixOp (..),
) where

import Data.Text (Text, unpack)

type Program = [Statement]

data Statement where
  Let :: {symbol :: Symbol, expr :: Expr} -> Statement
  Return :: {returnedExpr :: Expr} -> Statement
  ExprStmt :: {innerExpr :: Expr, semicolon :: Bool} -> Statement
  deriving (Eq, Show)

-- instance Show Statement where
--   show = \case
--     Let{..} -> "let " ++ show symbol ++ "="
--     _ -> undefined

data Expr where
  Null :: Expr
  Number :: Int -> Expr
  Bool :: Bool -> Expr
  SymbolExpr :: Symbol -> Expr
  Prefix :: {prefixOp :: PrefixOp, prefixBody :: Expr} -> Expr
  Infix :: {infixOp :: InfixOp, leftExp :: Expr, rightExp :: Expr} -> Expr
  If :: {cond :: Expr, consequence :: Program, alter :: Maybe Program} -> Expr
  Fn :: {params :: [Symbol], body :: Program} -> Expr
  Call :: {callExpr :: Expr, args :: [Expr]} -> Expr
  deriving (Eq)

instance Show Expr where
  show = \case
    Null -> "null"
    Number n -> show n
    Bool b -> if b then "true" else "false"
    SymbolExpr (Symbol t) -> unpack t
    Prefix{..} -> show prefixOp ++ show prefixBody
    Infix{..} -> show leftExp ++ show infixOp ++ show rightExp
    _ -> undefined

data PrefixOp = MinusPrefix | Not deriving (Eq)
instance Show PrefixOp where
  show = \case
    MinusPrefix -> "-"
    Not -> "!"

data InfixOp
  = Plus
  | Minus
  | Multiply
  | Divide
  | Lt
  | Gt
  | Eq
  | NotEq
  deriving (Eq)

instance Show InfixOp where
  show = \case
    Plus -> "+"
    Minus -> "-"
    Multiply -> "*"
    Divide -> "/"
    Lt -> "<"
    Gt -> ">"
    Eq -> "=="
    NotEq -> "!="

newtype Symbol = Symbol Text deriving (Eq)
instance Show Symbol where
  show (Symbol t) = unpack t