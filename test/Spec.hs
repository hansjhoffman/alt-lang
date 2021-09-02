{-# LANGUAGE OverloadedStrings #-}

module Main where
    
import Test.Hspec (hspec)

import LexerSpec
import ParserSpec


-- Main


main :: IO ()
main = 
    hspec $ do
        -- Lexer
        integerSpec
        floatSpec
        stringLiteralSpec
        binarySpec
        hexadecimalSpec
        octalSpec
        -- Parser
        pBooleanSpec
        pExprSpec
        pFloatSpec
        pIntegerSpec
