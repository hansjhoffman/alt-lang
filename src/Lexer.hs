{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Types


sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: String -> Parser String
symbol = L.symbol sc


-- Values


integer :: Parser Int
integer = lexeme (L.signed sc L.decimal)


float :: Parser Double
float = lexeme (L.signed sc L.float)


binary :: Parser Int
binary = char '0' >> char 'b' >> L.binary


octal :: Parser Int
octal = char '0' >> char 'o' >> L.octal


hexadecimal :: Parser Int
hexadecimal = char '0' >> char 'x' >> L.hexadecimal


-- Strings


stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"') <* sc


-- Punctuation


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


curlyBrackets :: Parser a -> Parser a
curlyBrackets = between (symbol "{") (symbol "}")


squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")


comma :: Parser String
comma = symbol ","


-- Identifier


identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)
