{-# LANGUAGE OverloadedStrings #-}

module Lexer where
    
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text


spaceConsumer :: Parser ()
spaceConsumer = 
    L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")


lexeme :: Parser a -> Parser a
lexeme = 
    L.lexeme spaceConsumer


integer :: Parser Integer
integer = 
    lexeme L.decimal
  

signedInteger :: Parser Integer
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
