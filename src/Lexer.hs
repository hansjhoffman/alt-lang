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


lineComment :: Parser ()
lineComment = L.skipLineComment "--"


blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"


sc :: Parser ()
sc = L.space space1 lineComment blockComment
-- sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment


-- scn :: Parser ()
-- scn = L.space space1 lineComment blockComment


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: Text -> Parser Text
symbol = L.symbol sc


integerLiteral :: Parser Integer
integerLiteral = lexeme L.decimal


signedInteger :: Parser Integer
signedInteger = L.signed sc integerLiteral


floatLiteral :: Parser Double
floatLiteral = lexeme L.float


signedFloat :: Parser Double
signedFloat = L.signed sc floatLiteral


stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')


binaryLiteral :: Parser Integer
binaryLiteral = char '0' >> char 'b' >> L.binary


octalLiteral :: Parser Integer
octalLiteral = char '0' >> char 'o' >> L.octal


hexadecimalLiteral :: Parser Integer
hexadecimalLiteral = char '0' >> char 'x' >> L.hexadecimal
