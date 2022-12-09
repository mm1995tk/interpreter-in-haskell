{-# LANGUAGE DuplicateRecordFields #-}
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
  getInfixPrecedence,
  PrecedenceOfInfixOp(..)
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
  | FnExpr {params :: [Identifier], body :: Program}
  | CallExpr {called :: Expr, args :: [Expr]}
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

getInfixPrecedence :: InfixOp -> PrecedenceOfInfixOp
getInfixPrecedence = \case
  Plus -> Sum
  Minus -> Sum
  Multiply -> Product
  Divide -> Product
  Lt -> LessOrGreater
  Gt -> LessOrGreater
  Eq -> Equals
  NotEq -> Equals

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

data PrecedenceOfInfixOp
  = Lowest
  | Equals
  | LessOrGreater
  | Sum
  | Product
  | Prefix
  deriving (Show, Eq, Ord)
