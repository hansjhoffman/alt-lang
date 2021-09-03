{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Monad.Combinators.Expr           ( Operator(..)
                                                          , makeExprParser
                                                          )
import           Data.Functor
import           Text.Megaparsec                          ( (<|>)
                                                          , choice
                                                          , eof
                                                          , many
                                                          , sepBy
                                                          )
import           Text.Megaparsec.Char

import           Lexer
import           Types


data Expr
    = LStr String
    | LInt Int
    | LFloat Double
    | LBool Bool
    | LUnit
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


data Stmt
    = ExprStmt Expr
    | IfStmt Expr [Stmt]
    deriving (Eq, Show)


data Collection
    = CList [Expr]
    | CTuple [Expr]
    deriving (Eq, Show)


type Program = [Stmt]


-- Values


pBoolean :: Parser Expr
pBoolean = (string "True" $> LBool True) <|> (string "False" $> LBool False)


pInteger :: Parser Expr
pInteger = LInt <$> integer


pFloat :: Parser Expr
pFloat = LFloat <$> float


pString :: Parser Expr
pString = LStr <$> stringLiteral


pBinary :: Parser Expr
pBinary = LInt <$> binary


pOctal :: Parser Expr
pOctal = LInt <$> octal


pHexadecimal :: Parser Expr
pHexadecimal = LInt <$> hexadecimal


pUnit :: Parser Expr
pUnit = symbol "(" *> symbol ")" $> LUnit


pValue :: Parser Expr
pValue =
    pString
        <|> pBoolean
        <|> pFloat
        <|> pInteger
        <|> pBinary
        <|> pOctal
        <|> pHexadecimal


-- Identifier


pIdentifier :: Parser Expr
pIdentifier = Var <$> identifier


-- Statements


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


-- Collections


pList :: Parser Collection
pList = CList <$> squareBrackets (sepBy pValue comma)


pTuple :: Parser Collection
pTuple = CTuple <$> parens (sepBy pValue comma)


pCollection :: Parser Collection
pCollection = pList <|> pTuple


-- Expressions


pLambda :: Parser Expr
pLambda = undefined


pTerm :: Parser Expr
pTerm = choice [parens pExpr, pValue, pIdentifier]


pExpr :: Parser Expr
pExpr = makeExprParser pTerm operators


operators :: [[Operator Parser Expr]]
operators =
    [ [ binaryOp Product $ symbol "*"
      , binaryOp Quotient $ symbol "/"
      , binaryOp Modulo $ symbol "mod"
      ]
    , [binaryOp Sum $ symbol "+", binaryOp Difference $ symbol "-"]
    , [ binaryOp GreaterThanEquals $ symbol ">="
      , binaryOp LessThanEquals $ symbol "<="
      , binaryOp LessThan $ symbol "<"
      , binaryOp GreaterThan $ symbol ">"
      ]
    , [binaryOp Equals $ symbol "==", binaryOp NotEquals $ symbol "!="]
    , [ binaryOp And $ symbol "and"
      , binaryOp Or $ symbol "or"
      , binaryOp In $ symbol "in"
      ]
    ]
    where binaryOp op symP = InfixL $ Binary op <$ symP


-- Program


pProgram :: Parser [Expr]
pProgram = sc *> many pTerm <* eof
