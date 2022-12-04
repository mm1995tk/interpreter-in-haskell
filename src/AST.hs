{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module AST (
  Expr (..),
  Program,
  Statement (..),
  Identifier (..),
  InfixOp (..),
  PrefixOp (..),
  Literal (..),
  Expression (toExpr, mapToExpr),
) where

import Data.Text (Text)
import Support.TypeClass (Display (..))

type Program = [Statement]

data Statement
  = Let {ident :: Identifier, expr :: Expr}
  | Return Expr
  | ExprStmt {expr :: Expr, isSemicolon :: Bool}
  deriving (Show, Eq)

class Expression a where
  toExpr :: a -> Expr
  mapToExpr :: (Applicative f) => f a -> f Expr
  mapToExpr = fmap toExpr

data Expr
  = LiteralExpr Literal
  | IdentExpr Identifier
  | PrefixExpr {prefixOp :: PrefixOp, expr :: Expr}
  | InfixExpr {infixOp :: InfixOp, leftExpr :: Expr, rightExpr :: Expr}
  | IfExpr {cond :: Expr, consequence :: Program, alter :: Maybe Program}
  | FnExpr Fn
  | CallExpr Call
  deriving (Show, Eq)

data Literal
  = NumLiteral Int
  | BoolLiteral Bool
  | Null
  deriving (Eq, Show)
instance Expression Literal where
  toExpr = LiteralExpr

newtype Identifier = Identifier Text deriving (Eq, Show, Ord)
instance Display Identifier where
  displayText (Identifier t) = t
instance Expression Identifier where
  toExpr = IdentExpr

data PrefixOp = MinusPrefix | Not deriving (Eq, Show)
instance Display PrefixOp where
  displayText = \case
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
  deriving (Eq, Show, Ord)
instance Display InfixOp where
  displayText = \case
    Plus -> "+"
    Minus -> "-"
    Multiply -> "*"
    Divide -> "/"
    Lt -> "<"
    Gt -> ">"
    Eq -> "=="
    NotEq -> "!="

data Fn = Fn {params :: [Identifier], body :: Program} deriving (Show, Eq)
instance Expression Fn where
  toExpr = FnExpr

data Call = Call {called :: CalledFunc, params :: [Expr]} deriving (Show, Eq)
instance Expression Call where
  toExpr = CallExpr

data CalledFunc
  = -- | 即時関数
    Iife Fn
  | --　| 高階関数
    HigherOrderFn Call
  | -- |通常呼び出し
    CallByIdent Identifier
  deriving (Show, Eq)
