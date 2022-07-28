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
  | RBinary RBinOp Expr Expr
  | Value Primative
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
  = Equal
  -- ^ (==)
  | GreaterThan
  -- ^ (>)
  | GreaterThanEqual
  -- ^ (>=)
  | LessThan
  -- ^ (<)
  | LessThanEqual
  -- ^ (<=)
  | NotEqual
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

data Primative
  = BoolLiteral Bool
  | FloatLiteral Double
  | IntLiteral Integer
  | StringLiteral String
  deriving (Eq, Show)


-- Statements

-- data Stmt
--   = Assign Text AExpr
--   | If BExpr Stmt Stmt
--   deriving (Eq, Show)


-- Values


pValue :: Parser Expr
pValue = pString <|> pInteger <|> pBinary <|> pOctal <|> pHexadecimal <|> pBoolean


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


pBoolean :: Parser Expr
pBoolean = Value . BoolLiteral <$> (string "True" $> True <|> string "False" $> False)


-- Main


pTerm :: Parser Expr
pTerm = choice [parens pExpr, pValue]


pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable


operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [Prefix (Negate <$ symbol "-"), Prefix (id <$ symbol "+")]
  , [ InfixL (RBinary Equal <$ symbol "==")
    , InfixL (RBinary NotEqual <$ symbol "/=")
    , InfixL (RBinary LessThanEqual <$ symbol "<=")
    , InfixL (RBinary GreaterThanEqual <$ symbol ">=")
    , InfixL (RBinary LessThan <$ symbol "<")
    , InfixL (RBinary GreaterThan <$ symbol ">")
    ]
  , [InfixL (ABinary Exponent <$ symbol "^")]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide <$ symbol "/")
    , InfixL (ABinary Modulo <$ symbol "%")
    ]
  , [InfixL (ABinary Add <$ symbol "+"), InfixL (ABinary Subtract <$ symbol "-")]
  ]
