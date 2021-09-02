module Lexer
    ( integer
    )
    where

import Data.Char

import Types


char :: String -> String
char input =
    undefined


-- integer :: Parser Int
-- integer =
--     Parser $ \input ->
--         let (token, rest) = T.span isNumber (T.pack input)
--         -- in Just (read token :: Int, rest)
--         -- in Just (rest, read token :: Int)
--         in _a
integer :: String -> (String, String)
integer input =
    span isNumber input


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


-- Skip zero or more white space characters.
space :: String -> String
space input =
    takeWhile isSpace input


-- Skip one or more white space characters.
space1 :: String -> String
space1 input =
    takeWhile isSpace input
