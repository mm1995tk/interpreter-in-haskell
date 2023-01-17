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
  getInfixPrecedence,
  PrecedenceOfInfixOp (..),
) where

import qualified Data.Map as M
import Data.Text (Text)
import Support.TypeClass (Display (..))

type Program = [Statement]

data Statement
  = Let {ident :: Identifier, expr :: Expr}
  | Return Expr
  | ExprStmt {expr :: Expr, isSemicolon :: Bool}
  deriving (Show, Eq, Ord)

data Expr
  = LiteralExpr Literal
  | ArrExpr [Expr]
  | HashMapExpr (M.Map Expr Expr)
  | AccessExpr {target :: Expr, accessor :: Expr}
  | IdentExpr Identifier
  | PrefixExpr {prefixOp :: PrefixOp, expr :: Expr}
  | InfixExpr {infixOp :: InfixOp, leftExpr :: Expr, rightExpr :: Expr}
  | IfExpr {cond :: Expr, consequence :: Program, alter :: Maybe Program}
  | FnExpr {params :: [Identifier], body :: Program}
  | CallExpr {called :: Expr, args :: [Expr]}
  deriving (Show, Eq, Ord)

data Literal
  = NumLiteral Int
  | StrLiteral Text
  | BoolLiteral Bool
  | Null
  deriving (Eq, Show, Ord)

newtype Identifier = Identifier Text deriving (Eq, Show, Ord)
instance Display Identifier where
  displayText (Identifier t) = t

data PrefixOp = MinusPrefix | Not deriving (Eq, Show, Ord)
instance Display PrefixOp where
  displayText = \case
    MinusPrefix -> "-"
    Not -> "!"

data InfixOp
  = Plus
  | Minus
  | Multiply
  | Divide
  | Mod
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
  Mod -> Product
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
    Mod -> "%"
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
