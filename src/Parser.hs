{-# LANGUAGE OverloadedStrings #-}

module Parser where
  
import Control.Monad.Combinators.Expr
    ( Operator(..)
    , makeExprParser
    )
import Data.Functor
import Text.Megaparsec ((<|>), choice)
import Text.Megaparsec.Char

import Lexer
import Types


data Expr
    = LStr String
    | LInt Int
    | LFloat Double
    | LBool Bool
    | Var Identifier
    | Binary BinOp Expr Expr
    deriving (Eq, Show)


type Identifier = String


data BinOp
    = Plus
    | Minus
    | Mult
    | Div
    | Equals
    | NotEquals
    | LessThan
    | GreaterThan
    | LessThanEquals
    | GreaterThanEquals
    deriving (Eq, Show)


data Op
    = PipeForward
    | PipeBackward
    deriving (Eq, Show)


data Stmt
    = ExprStmt Expr
    | IfStmt Expr [Stmt]
    deriving (Eq, Show)


type Program
    = [Stmt]

 
-- Values


pBoolean :: Parser Expr          
pBoolean =
    (string "True" $> LBool True) <|> 
    (string "False" $> LBool False)


pInteger :: Parser Expr
pInteger =
    LInt <$> integer
    

pFloat :: Parser Expr
pFloat =
    LFloat <$> float


pString :: Parser Expr
pString =
    LStr <$> stringLiteral


pBinary :: Parser Expr
pBinary =
    LInt <$> binary


pOctal :: Parser Expr
pOctal =
    LInt <$> octal


pHexadecimal :: Parser Expr
pHexadecimal =
    LInt <$> hexadecimal


pValue :: Parser Expr
pValue =
    pString <|>
    pBoolean <|>
    pInteger <|>
    pFloat <|>
    pBinary <|>
    pOctal <|>
    pHexadecimal


-- Identifier


pIdentifier :: Parser Expr
pIdentifier =
    Var <$> identifier


-- Statements


-- pLetStmt :: Parser Stmt
-- pLetStmt =
--     undefined


-- pIfStmt :: Parser Stmt
-- pIfStmt =
--     IfStmt <$> (symbol "if" *> pExpr) <*> (symbol "then") <*> (many pStmt) <*> (symbol "else") <*> (many pStmt)


-- pStmt :: Parser Stmt
-- pStmt =
--     pIfStmt <|>
--     pLetStmt


-- Expressions


pLambda :: Parser Expr
pLambda =
    undefined
    

pTerm :: Parser Expr
pTerm =
    choice
        [ parens pExpr
        , pValue
        , pIdentifier
        ]


pExpr :: Parser Expr
pExpr =
    makeExprParser pTerm operators
    

operators :: [[Operator Parser Expr]]
operators =
    [
        [ binary Mult $ symbol "*"
        , binary Div $ symbol "/"
        ]
    ,   [ binary Plus $ symbol "+"
        , binary Minus $ symbol "-"
        ]
    ,   [ binary LessThan $ symbol "<"
        , binary GreaterThan $ symbol ">"
        ]
    ,   [ binary Equals $ symbol "=="
        , binary NotEquals $ symbol "!="
        ]
    -- ,   [ binary GreaterThanEquals $ symbol ">="
    --     , binary LessThanEquals $ symbol "<="
    --     ]
    -- ,   [ binary PipeForward $ symbol "|>"
    --     , binary PipeBackward $ symbol "<|"
    --     ]
    ]
    where
        binary op symP = InfixL $ Binary op <$ symP
