{-# LANGUAGE OverloadedStrings #-}

module Parser where
  
import Control.Monad.Combinators.Expr
    ( Operator(..)
    , makeExprParser
    )
import Data.Functor
import Text.Megaparsec ((<|>), choice, eof, many)
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
    = Sum
    | Difference
    | Product
    | Quotient
    | Modulo
    | And
    | Or
    | In
    | Equals
    | NotEquals
    | LessThan
    | GreaterThan
    | LessThanEquals
    | GreaterThanEquals
    deriving (Eq, Show)


data InfixOp
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
        [ binaryOp Product $ symbol "*"
        , binaryOp Quotient $ symbol "/"
        , binaryOp Modulo $ symbol "mod"
        ]
    ,   [ binaryOp Sum $ symbol "+"
        , binaryOp Difference $ symbol "-"
        ]
    ,   [ binaryOp LessThan $ symbol "<"
        , binaryOp GreaterThan $ symbol ">"
        ]
    ,   [ binaryOp Equals $ symbol "=="
        , binaryOp NotEquals $ symbol "!="
        ]
    ,   [ binaryOp And $ symbol "and"
        , binaryOp Or $ symbol "or"
        , binaryOp In $ symbol "in"
        ]
    -- ,   [ binary GreaterThanEquals $ symbol ">="
    --     , binary LessThanEquals $ symbol "<="
    --     ]
    -- ,   [ InfixL $ PipeForward $ symbol "|>"
    --     , InfixL $ PipeBackward $ symbol "<|"
    --     ]
    ]
    where
        binaryOp op symP = InfixL $ Binary op <$ symP


-- Program


pProgram :: Parser [Expr]
pProgram =
    sc *> many pTerm <* eof
