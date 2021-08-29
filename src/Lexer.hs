{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text


data Expr
    = Var String
    | Int Int
    | Bool Bool
    | Negation    Expr
    | Addition    Expr Expr
    | Subtraction Expr Expr
    | Product     Expr Expr
    | Division    Expr Expr
    deriving (Eq, Ord, Show)


spaceConsumer :: Parser ()
spaceConsumer = 
    L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")


lexeme :: Parser a -> Parser a
lexeme = 
    L.lexeme spaceConsumer
    

symbol :: Text -> Parser Text
symbol =
    L.symbol spaceConsumer


integer :: Parser Int
integer = 
    lexeme L.decimal
  

signedInteger :: Parser Int
signedInteger =
    L.signed spaceConsumer integer


float :: Parser Double
float = 
    lexeme L.float
  

signedFloat :: Parser Double
signedFloat =
    L.signed spaceConsumer float


stringLiteral :: Parser String
stringLiteral =
    char '\"' *> manyTill L.charLiteral (char '\"')
    
    
parens :: Parser a -> Parser a
parens =
    between (symbol "(") (symbol ")")


squareBrackets :: Parser a -> Parser a
squareBrackets =
    between (symbol "[") (symbol "]")
    

comma :: Parser Text
comma =
    symbol ","
    

pBoolean :: Parser Expr
pBoolean =
    (trueParser >> return (Bool True)) <|> 
    (falseParser >> return (Bool False))
        where
            trueParser = string "True"
            falseParser = string "False"
    

pInteger :: Parser Expr
pInteger =
    Int <$> integer
    

pTerm :: Parser Expr
pTerm =
    choice
        [ parens pExpr
        , pInteger
        ]


pExpr :: Parser Expr
pExpr =
    makeExprParser pTerm operatorTable
    

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [
        [ prefix "-" Negation
        , prefix "+" id
        ]
    ,   [ binary "*" Product
        , binary "/" Division
        ]
    ,   [ binary "+" Addition
        , binary "-" Subtraction
        ]
    ]


binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f =
    InfixL (f <$ symbol name)
    

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f =
    Prefix (f <$ symbol name)
    

postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
postfix name f =
    Postfix (f <$ symbol name)
