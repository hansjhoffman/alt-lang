{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Types


-- Helpers


spaceConsumer :: Parser ()
spaceConsumer = 
    L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")


lexeme :: Parser a -> Parser a
lexeme = 
    L.lexeme spaceConsumer
    

symbol :: Text -> Parser Text
symbol =
    L.symbol spaceConsumer


-- Numbers


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


-- Strings


stringLiteral :: Parser String
stringLiteral =
    char '\"' *> manyTill L.charLiteral (char '\"')
    

-- Punctuation


parens :: Parser a -> Parser a
parens =
    between (symbol "(") (symbol ")")


squareBrackets :: Parser a -> Parser a
squareBrackets =
    between (symbol "[") (symbol "]")
    

comma :: Parser Text
comma =
    symbol ","
