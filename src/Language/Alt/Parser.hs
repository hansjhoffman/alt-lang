module Language.Alt.Parser where

import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )
import           Language.Alt.Lexer
import           Language.Alt.Types
import           RIO
import           Text.Megaparsec


data Expr
  = ABinary ABinOp Expr Expr
  | LBinary LBinOp Expr Expr
  | Negate Expr
  | Not Expr
  | RBinary RBinOp Expr Expr
  | Value Literal
  deriving (Eq, Show)


-- Arithmetic Binary Operators

data ABinOp
  = Add
  -- ^ (+)
  | Divide
  -- ^ (/)
  | Exponent
  -- ^ (^)
  | Modulus
  -- ^ (%)
  | Multiply
  -- ^ (*)
  | Subtract
  -- ^ (-)
  deriving (Eq, Show)


-- Relational Binary Operators

data RBinOp
  = EqualTo
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
  deriving (Eq, Show)


-- Logical Binary Operators

data LBinOp
  = And
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


-- Statements

-- data Stmt
--   = IfStmt Expr Stmt Stmt
--   | LetBlock Expr
--   deriving (Eq, Show)


-- pLetStmt :: Parser Stmt
-- pLetStmt = undefined


-- pIfStmt :: Parser Stmt
-- pIfStmt =
--     IfStmt
--         <$> (symbol "if" *> pExpr)
--         <*> (symbol "then")
--         <*> (many pStmt)
--         <*> (symbol "else")
--         <*> (many pStmt)


-- pStmt :: Parser Stmt
-- pStmt = pIfStmt <|> pLetStmt


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
pBoolean = Value . BooleanLiteral <$> booleanLiteral


-- Main


pTerm :: Parser Expr
pTerm = choice [parens pExpr, pValue]


pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable


operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [Prefix (Negate <$ symbol "-"), Prefix (id <$ symbol "+"), Prefix (Not <$ symbol "not")]
  , [ InfixL (RBinary EqualTo <$ symbol "==")
    , InfixL (RBinary NotEqualTo <$ symbol "/=")
    , InfixL (RBinary LessThanOrEqualTo <$ symbol "<=")
    , InfixL (RBinary GreaterThanOrEqualTo <$ symbol ">=")
    , InfixL (RBinary LessThan <$ symbol "<")
    , InfixL (RBinary GreaterThan <$ symbol ">")
    ]
  , [InfixL (ABinary Exponent <$ symbol "^")]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide <$ symbol "/")
    , InfixL (ABinary Modulus <$ symbol "%")
    ]
  , [ InfixL (LBinary And <$ symbol "and")
    , InfixL (LBinary Or <$ symbol "or")
    , InfixL (LBinary Xor <$ symbol "xor")
    ]
  , [InfixL (ABinary Add <$ symbol "+"), InfixL (ABinary Subtract <$ symbol "-")]
  ]
