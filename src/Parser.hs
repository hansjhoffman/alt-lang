module Parser where

import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )
import           Lexer
import           RIO
import           Text.Megaparsec
import           Types


data Expr
  = Negate Expr
  | ABinary ABinOp Expr Expr
  | IntLiteral Integer
  | FloatLiteral Double
  | StringLiteral String
  deriving (Eq, Show)


-- Arithmetic Binary Operators

data ABinOp
  = Add
  | Divide
  | Exponent
  | Modulo
  | Multiply
  | Subtract
  deriving (Eq, Show)


-- Relational Binary Operators

data RBinOp
  = EQ
  | NE
  | LT
  | GT
  | LTE
  | GTE
  deriving (Eq, Show)


-- Statements

-- data Stmt
--   = Assign Text AExpr
--   | If BExpr Stmt Stmt
--   deriving (Eq, Show)


-- Symbols


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")


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



pTerm :: Parser Expr
pTerm = choice [parens pExpr, pString, pInteger, pBinary, pHexadecimal, pOctal, pFloat]


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
