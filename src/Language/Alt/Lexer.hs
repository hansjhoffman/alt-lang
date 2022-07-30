module Language.Alt.Lexer where

import           Language.Alt.Types
import           RIO                     hiding ( try )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


lineComment :: Parser ()
lineComment = L.skipLineComment "--"


blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"


sc :: Parser ()
sc = L.space space1 lineComment blockComment


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: Text -> Parser Text
symbol = L.symbol sc


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")


numericLiteral :: Parser Double
numericLiteral = lexeme $ try L.float <|> L.decimal


stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' *> manyTill L.charLiteral (char '"')


charLiteral :: Parser Char
charLiteral = lexeme L.charLiteral


binaryLiteral :: Parser Double
binaryLiteral = lexeme $ char '0' >> char 'b' >> L.binary


octalLiteral :: Parser Double
octalLiteral = lexeme $ char '0' >> char 'o' >> L.octal


hexadecimalLiteral :: Parser Double
hexadecimalLiteral = lexeme $ char '0' >> char 'x' >> L.hexadecimal


booleanLiteral :: Parser Bool
booleanLiteral = lexeme $ string "True" $> True <|> string "False" $> False
