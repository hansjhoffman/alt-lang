module Lexer where

import           Control.Applicative            ( (*>) )
import           Control.Monad                  ( (>>) )
import           Data.Text                      ( Text )
import           Prelude                        ( Double
                                                , Integer
                                                , String
                                                )
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


symbol :: Text -> Parser Text
symbol = L.symbol sc


integer :: Parser Integer
integer = lexeme L.decimal


signedInteger :: Parser Integer
signedInteger = L.signed sc integer


float :: Parser Double
float = lexeme L.float


signedFloat :: Parser Double
signedFloat = L.signed sc float


stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')


binary :: Parser Integer
binary = char '0' >> char 'b' >> L.binary


octal :: Parser Integer
octal = char '0' >> char 'o' >> L.octal


hexadecimal :: Parser Integer
hexadecimal = char '0' >> char 'x' >> L.hexadecimal
