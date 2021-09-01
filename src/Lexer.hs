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
    L.space space1 lineComment blockComment
    where
        lineComment = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"


lexeme :: Parser a -> Parser a
lexeme = 
    L.lexeme spaceConsumer
    

symbol :: Text -> Parser Text
symbol =
    L.symbol spaceConsumer


-- Values


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


-- binary' :: Parser Integer
-- binary' =
--     char '0' >> char 'b' >> L.binary


-- octal' :: Parser Integer
-- octal' =
--     char '0' >> char 'o' >> L.octal


-- hexadecimal' :: Parser Int
-- hexadecimal' =
--     char '0' >> char 'x' >> L.hexadecimal


-- Strings


stringLiteral :: Parser String
stringLiteral =
    char '"' *> manyTill L.charLiteral (char '"')
    

-- Punctuation


parens :: Parser a -> Parser a
parens =
    between (symbol "(") (symbol ")")


curlyBrackets :: Parser a -> Parser a
curlyBrackets =
    between (symbol "{") (symbol "}")


squareBrackets :: Parser a -> Parser a
squareBrackets =
    between (symbol "[") (symbol "]")
    

comma :: Parser Text
comma =
    symbol ","


-- commaSep :: Parser a -> Parser [a]
-- commaSep p =
--     sepBy p comma
