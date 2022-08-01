module Language.Alt.AST.AST where

import           RIO


data UnaryOperator
  = Negate
  | Not
  deriving (Eq, Show)


data BinaryOperator
  = Add
  -- ^ `+`
  | Divide
  -- ^ `/`
  | Multiply
  -- ^ `*`
  | Sequence
  -- ^ `..`
  | Subtract
  -- ^ `-`
  | EqualTo
  -- ^ `==`
  | GreaterThan
  -- ^ `>`
  | GreaterThanOrEqualTo
  -- ^ `>=`
  | LessThan
  -- ^ `<`
  | LessThanOrEqualTo
  -- ^ `<=`
  | NotEqualTo
  -- ^ `!=`
  | And
  -- ^ `and`
  | Or
  -- ^ `or`
  deriving (Eq, Show)


data Literal
  = BooleanLiteral Bool
  -- ^ A boolean literal
  | CharLiteral Char
  -- ^ A character literal
  | NumericLiteral Double
  -- ^ An integer/float literal
  | StringLiteral String
  -- ^ A string literal
  deriving (Eq, Show)


data AST
  = Binary BinaryOperator AST AST
  | Unary UnaryOperator AST
  | Value Literal
  deriving (Eq, Show)
