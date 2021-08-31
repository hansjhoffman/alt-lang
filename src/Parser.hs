{-# LANGUAGE OverloadedStrings #-}

module Parser where
  
import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Text (Text)
import Text.Megaparsec ((<|>), choice)
import Text.Megaparsec.Char

import Lexer
import Types


data Expr
    = Var String
    | Int Int
    | Float Double
    | Boolean Bool
    | Negation    Expr
    | Addition    Expr Expr
    | Subtraction Expr Expr
    | Product     Expr Expr
    | Division    Expr Expr
    deriving (Eq, Ord, Show)


pBoolean :: Parser Expr          
pBoolean =
    (string "True" $> Boolean True) <|> 
    (string "False" $> Boolean False)


pInteger :: Parser Expr
pInteger =
    Int <$> integer
    

pFloat :: Parser Expr
pFloat =
    Float <$> float


pLet :: Parser Expr
pLet =
    undefined
    

pIf :: Parser Expr
pIf =
    undefined
    

pLambda :: Parser Expr
pLambda =
    undefined
    

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
