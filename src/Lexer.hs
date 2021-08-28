module Lexer where
    
import Text.Parsec.String (Parser)


integer :: Parser Integer
integer = Tok.integer lexer 


float :: Parser Double
float = Tok.float lexer 


identifier :: Parser String
identifier = Tok.identifier lexer 


parens :: Parser a -> Parser a
parens = Tok.parens lexer 