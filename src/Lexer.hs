module Lexer
    ( integer
    )
    where

import Data.Char

import Types


data Token
    = TInt Int
    deriving (Eq, Show)


integer :: String -> Maybe Int
integer input =
    case lexeme of
        [] ->
            Nothing
            
        lexeme' ->
            Just (read lexeme' :: Int)
    where
        lexeme = takeWhile isNumber input


hexadecimal :: String -> String
hexadecimal input =
    takeWhile isHexDigit input


octal :: String -> String
octal input =
    takeWhile isOctDigit input


binary :: String -> String
binary input =
    takeWhile isBinDigit input
    where
        isBinDigit x =
            x == '0' || x == '1'


string :: String -> String
string input =
    takeWhile isAlpha input


-- signed :: Parser a -> Text
-- signed =
--     undefined
