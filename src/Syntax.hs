module Syntax where


data Expr
  = Float Double
  deriving (Eq, Ord, Show)


data Op
  = Plus
  | Minus
  | Mult
  | Div
  deriving (Eq, Ord, Show)