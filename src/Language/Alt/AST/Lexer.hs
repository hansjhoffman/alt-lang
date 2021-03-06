module Language.Alt.AST.Lexer where

import           Language.Alt.AST.Types
import           RIO                     hiding ( many
                                                , try
                                                )
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


squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")


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


comma :: Parser Text
comma = lexeme $ symbol ","


upperName :: Parser String
upperName = lexeme $ (:) <$> upperChar <*> many letterChar


lowerName :: Parser String
lowerName = lexeme $ (:) <$> lowerChar <*> many letterChar
