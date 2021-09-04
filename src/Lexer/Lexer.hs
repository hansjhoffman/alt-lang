module Lexer.Lexer where

import           Control.Applicative
import qualified Data.Text                     as T


data LexerError
    = Unexpected Char
    | GotEOF
    deriving (Eq, Show)


data Token
    = If -- `if`
    | Then -- `then`
    | Else -- `else`
    | Match -- `match`
    | On -- `on`
    | Let -- `let`
    | In -- `in`
    | Type -- `type`
    | Underscore -- `_`
    | OpenParens -- `(`
    | CloseParens -- `)`
    | OpenBracket -- `[`
    | CloseBracket -- `]`
    | DoubleColon -- `::`
    | PipeForward -- `|>`
    | PipeBackward -- `<|`
    | Dot -- `.`
    | ThinArrow -- `->`
    | FatArrow -- `=>`
    | Plus -- `+`
    | PlusPlus -- `++`
    | Dash -- `-`
    | Asterisk -- `*`
    | ForwardSlash -- `/`
    | Equal -- `=`
    | EqualEqual -- `==`
    | BangEqual -- `!=`
    | LeftAngle -- `<`
    | LeftAngleEqual -- `<=`
    | RightAngle -- `>`
    | RightAngleEqual -- `>=`
    -- A Int literal
    | IntLit Int
    -- A Float literal
    | FloatLit Float
    -- A Char literal
    | CharLit Char
    -- A Bool literal
    | BoolLit Bool
    -- A String literal
    | StrLit String
    -- A name starting with a uppercase letter
    | UpperName String
    -- A name starting with a lowercase letter
    | LowerName String
    deriving (Eq, Show)


newtype Lexer a = Lexer { runLexer :: T.Text -> Either LexerError (a, T.Text) }


lexer :: T.Text -> Either LexerError [Token]
lexer _ = Left GotEOF


unexpected :: T.Text -> LexerError
unexpected input = case T.unpack input of
    []      -> GotEOF
    (c : _) -> Unexpected c


satisfy :: (Char -> Bool) -> Lexer Char
satisfy predicate = do
    Lexer $ \input -> case T.unpack input of
        (c : cs) | predicate c -> Right (c, T.pack cs)
        rest                   -> Left <$> unexpected $ T.pack rest


char :: Char -> Lexer Char
char = undefined


string :: Lexer T.Text
string = undefined


oneOf :: [f a] -> f a
oneOf = undefined


keyword :: Lexer (Token, T.Text)
keyword = undefined


operator :: Lexer (Token, T.Text)
operator = undefined


intLit :: Lexer (Token, T.Text)
intLit = undefined


floatLit :: Lexer (Token, T.Text)
floatLit = undefined


boolLit :: Lexer (Token, T.Text)
boolLit = undefined


stringLit :: Lexer (Token, T.Text)
stringLit = undefined


literal :: Lexer (Token, T.Text)
literal = intLit <|> floatLit <|> boolLit <|> stringLit


typeName :: Lexer (Token, T.Text)
typeName = undefined


upperName :: Lexer (Token, T.Text)
upperName = undefined


lowerName :: Lexer (Token, T.Text)
lowerName = undefined


name :: Lexer (Token, T.Text)
name = typeName <|> upperName <|> lowerName


token :: Lexer (Token, T.Text)
token = keyword <|> operator <|> literal <|> name


-- Instances


instance Functor Lexer where
    fmap f (Lexer l) = Lexer $ \input -> do
        (a, remaining) <- l input
        Right (f a, remaining)


instance Applicative Lexer where
    pure a = Lexer $ \input -> Right (a, input)

    (Lexer lF) <*> (Lexer lA) = Lexer $ \input -> do
        (f, remaining ) <- lF input
        (a, remaining') <- lA remaining
        Right (f a, remaining')


instance Alternative Lexer where
    empty = Lexer (Left . unexpected)

    (Lexer lA) <|> (Lexer lB) = Lexer $ \input -> case lA input of
        Right (a, remaining) -> Right (a, remaining)
        Left  _              -> case lB input of
            Right (b, remaining') -> Right (b, remaining')
            Left  _               -> Left $ unexpected input
