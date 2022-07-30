module Language.Alt.Parser where

import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )
import           Language.Alt.Lexer
import           Language.Alt.Types
import           RIO
import           Text.Megaparsec


data Expr
  = Binary BinaryOperator Expr Expr
  | Unary UnaryOperator Expr
  | Value Literal
  deriving (Eq, Show)


data UnaryOperator
  = Negate
  | Not
  deriving (Eq, Show)


data BinaryOperator
  = Add
  -- ^ (+)
  | Divide
  -- ^ (/)
  | Multiply
  -- ^ (*)
  | Sequence
  -- ^ (..)
  | Subtract
  -- ^ (-)
  | EqualTo
  -- ^ (==)
  | GreaterThan
  -- ^ (>)
  | GreaterThanOrEqualTo
  -- ^ (>=)
  | LessThan
  -- ^ (<)
  | LessThanOrEqualTo
  -- ^ (<=)
  | NotEqualTo
  -- ^ (/=)
  | And
  -- ^ (and)
  | Or
  -- ^ (or)
  | Xor
  -- ^ (xor)
  deriving (Eq, Show)


-- Literal Types

data Literal
  = BooleanLiteral Bool
  -- ^ A boolean literal
  | CharLiteral Char
  -- ^ A character literal
  | FloatLiteral Double
  -- ^ A float literal
  | IntLiteral Integer
  -- ^ An integer literal
  | StringLiteral String
  -- ^ A string literal
  deriving (Eq, Show)


-- Values


pLiteral :: Parser Expr
pLiteral = pString <|> pInteger <|> pBinary <|> pOctal <|> pHexadecimal <|> pBoolean <|> pChar


pFloat :: Parser Expr
pFloat = Value . FloatLiteral <$> floatLiteral


pInteger :: Parser Expr
pInteger = Value . IntLiteral <$> integerLiteral


pBinary :: Parser Expr
pBinary = Value . IntLiteral <$> binaryLiteral


pOctal :: Parser Expr
pOctal = Value . IntLiteral <$> octalLiteral


pHexadecimal :: Parser Expr
pHexadecimal = Value . IntLiteral <$> hexadecimalLiteral


pString :: Parser Expr
pString = Value . StringLiteral <$> stringLiteral


pChar :: Parser Expr
pChar = Value . CharLiteral <$> charLiteral


pBoolean :: Parser Expr
pBoolean = Value . BooleanLiteral <$> booleanLiteral


-- Main


pTerm :: Parser Expr
pTerm = choice [parens pExpr, pLiteral]


pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable


operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix (Unary Negate <$ symbol "-")
    , Prefix (id <$ symbol "+")
    , Prefix (Unary Not <$ symbol "not")
    ]
  , [ InfixL (Binary EqualTo <$ symbol "==")
    , InfixL (Binary NotEqualTo <$ symbol "/=")
    , InfixL (Binary LessThanOrEqualTo <$ symbol "<=")
    , InfixL (Binary GreaterThanOrEqualTo <$ symbol ">=")
    , InfixL (Binary LessThan <$ symbol "<")
    , InfixL (Binary GreaterThan <$ symbol ">")
    ]
  , [ InfixL (Binary Add <$ symbol "+")
    , InfixL (Binary Subtract <$ symbol "-")
    , InfixL (Binary Multiply <$ symbol "*")
    , InfixL (Binary Divide <$ symbol "/")
    , InfixL (Binary Sequence <$ symbol "..")
    ]
  , [ InfixL (Binary And <$ symbol "and")
    , InfixL (Binary Or <$ symbol "or")
    , InfixL (Binary Xor <$ symbol "xor")
    ]
  ]
