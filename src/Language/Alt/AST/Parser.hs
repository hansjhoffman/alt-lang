module Language.Alt.AST.Parser where

import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )
import           Language.Alt.AST.AST
import           Language.Alt.AST.Lexer
import           Language.Alt.AST.Types
import           RIO
import           Text.Megaparsec


pNumeric :: Parser AST
pNumeric = Value . NumericLiteral <$> numericLiteral


pBinary :: Parser AST
pBinary = Value . NumericLiteral <$> binaryLiteral


pOctal :: Parser AST
pOctal = Value . NumericLiteral <$> octalLiteral


pHexadecimal :: Parser AST
pHexadecimal = Value . NumericLiteral <$> hexadecimalLiteral


pString :: Parser AST
pString = Value . StringLiteral <$> stringLiteral


pChar :: Parser AST
pChar = Value . CharLiteral <$> charLiteral


pBoolean :: Parser AST
pBoolean = Value . BooleanLiteral <$> booleanLiteral


pLiteral :: Parser AST
pLiteral = pString <|> pNumeric <|> pBinary <|> pOctal <|> pHexadecimal <|> pBoolean <|> pChar


-- Main


operatorTable :: [[Operator Parser AST]]
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


pTerm :: Parser AST
pTerm = choice [parens pExpr, pLiteral]


pExpr :: Parser AST
pExpr = makeExprParser pTerm operatorTable
