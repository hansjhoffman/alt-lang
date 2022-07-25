module Parser where

import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )
import           Lexer
import           RIO
import           Text.Megaparsec
import           Types


data Expr
  = Negation Expr
  | Sum Expr Expr
  | Subtract Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | IntLiteral Integer
  | FloatLiteral Double
  | StringLiteral String
  deriving (Eq, Show)


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
pTerm = choice [parens pExpr, pInteger, pBinary, pHexadecimal, pOctal, pFloat, pString]


pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable


operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix "-" Negation, prefix "+" id]
  , [binaryOp "*" Product, binaryOp "/" Division]
  , [binaryOp "+" Sum, binaryOp "-" Subtract]
  ]
 where
  binaryOp :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
  binaryOp name f = InfixL (f <$ symbol name)

  prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
  prefix name f = Prefix (f <$ symbol name)
  postfix name f = Postfix (f <$ symbol name)
