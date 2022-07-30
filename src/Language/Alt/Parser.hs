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


pLiteral :: Parser Expr
pLiteral = pString <|> pNumeric <|> pBinary <|> pOctal <|> pHexadecimal <|> pBoolean <|> pChar


pNumeric :: Parser Expr
pNumeric = Value . NumericLiteral <$> numericLiteral


pBinary :: Parser Expr
pBinary = Value . NumericLiteral <$> binaryLiteral


pOctal :: Parser Expr
pOctal = Value . NumericLiteral <$> octalLiteral


pHexadecimal :: Parser Expr
pHexadecimal = Value . NumericLiteral <$> hexadecimalLiteral


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
  [ [Prefix (Unary Negate <$ symbol "-"), Prefix (id <$ symbol "+")]
  , [ InfixL (Binary Multiply <$ symbol "*")
    , InfixL (Binary Divide <$ symbol "/")
    , InfixL (Binary Add <$ symbol "+")
    , InfixL (Binary Subtract <$ symbol "-")
    , InfixL (Binary Sequence <$ symbol "..")
    ]
  , [Prefix (Unary Not <$ symbol "not")]
  , [ InfixL (Binary LessThanOrEqualTo <$ symbol "<=")
    , InfixL (Binary LessThan <$ symbol "<")
    , InfixL (Binary GreaterThanOrEqualTo <$ symbol ">=")
    , InfixL (Binary GreaterThan <$ symbol ">")
    , InfixL (Binary EqualTo <$ symbol "==")
    , InfixL (Binary NotEqualTo <$ symbol "!=")
    ]
  , [InfixL (Binary And <$ symbol "and"), InfixL (Binary Or <$ symbol "or")]
  ]
