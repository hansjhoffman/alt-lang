module Lexer where
    
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as PT


lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser style
  where
    ops = ["+", "*", "-"]
    names = []
    style = emptyDef { PT.commentLine = "--"
                      , PT.reservedOpNames = ops
                      , PT.reservedNames = names
                      }


integerParser :: Parser Integer
integerParser = PT.integer lexer 


floatParser :: Parser Double
floatParser = PT.float lexer 


identifierParser :: Parser String
identifierParser = PT.identifier lexer 


parensParser :: Parser a -> Parser a
parensParser = PT.parens lexer 