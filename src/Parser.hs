module Parser where

import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )
import           Lexer
import           RIO
import           Text.Megaparsec
import           Text.Megaparsec.Char           ( string )
import           Types


data Expr
  = Negate Expr
  | ABinary ABinOp Expr Expr
  | BooleanLiteral Bool
  | IntLiteral Integer
  | FloatLiteral Double
  | StringLiteral String
  deriving (Eq, Show)


-- Arithmetic Binary Operators

data ABinOp
  = Add
  -- ^ (+)
  | Divide
  -- ^ (/)
  | Exponent
  -- ^ (^)
  | Modulo
  -- ^ (%)
  | Multiply
  -- ^ (*)
  | Subtract
  -- ^ (-)
  deriving (Eq, Show)


-- Relational Binary Operators

data RBinOp
  = EQ
  -- ^ (==)
  | GT
  -- ^ (>)
  | GTE
  -- ^ (>=)
  | LT
  -- ^ (<)
  | LTE
  -- ^ (<=)
  | NE
  -- ^ (/=)
  deriving (Eq, Show)


-- Logical Binary Operators

data LBinOp
  = And
  -- ^ and
  | Or
  -- ^ or
  | Not
  -- ^ not
  | Xor
  -- ^ xor
  deriving (Eq, Show)


-- Primative Data Types

-- data Primative
--   = BoolLiteral Bool
--   | FloatLiteral Double
--   | IntLiteral Integer
--   | StringLiteral String


-- Statements

-- data Stmt
--   = Assign Text AExpr
--   | If BExpr Stmt Stmt
--   deriving (Eq, Show)


-- Values


pFloat :: Parser Expr
pFloat = FloatLiteral <$> floatLiteral


pInteger :: Parser Expr
pInteger = IntLiteral <$> integerLiteral


pBinary :: Parser Expr
pBinary = IntLiteral <$> binaryLiteral


pOctal :: Parser Expr
pOctal = IntLiteral <$> octalLiteral


pHexadecimal :: Parser Expr
pHexadecimal = IntLiteral <$> hexadecimalLiteral


pString :: Parser Expr
pString = StringLiteral <$> stringLiteral


pBoolean :: Parser Expr
pBoolean = BooleanLiteral <$> (string "True" $> True <|> string "False" $> False)


pTerm :: Parser Expr
pTerm = choice [parens pExpr, pString, pBoolean, pInteger, pBinary, pHexadecimal, pOctal, pFloat]


pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable


operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [Prefix (Negate <$ symbol "-"), Prefix (id <$ symbol "+")]
  , [InfixL (ABinary Exponent <$ symbol "^")]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide <$ symbol "/")
    , InfixL (ABinary Modulo <$ symbol "%")
    ]
  , [InfixL (ABinary Add <$ symbol "+"), InfixL (ABinary Subtract <$ symbol "-")]
  ]
