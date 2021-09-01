{-# LANGUAGE OverloadedStrings #-}

module Main where
    
import Test.Hspec (hspec)

import LexerSpec (integerSpec, floatSpec, stringLiteralSpec)
import ParserSpec (pBooleanSpec, pFloatSpec, pExprSpec, pIntegerSpec)


-- Main


main :: IO ()
main = 
    hspec $ do
        -- Lexer
        integerSpec
        floatSpec
        stringLiteralSpec
        -- Parser
        pBooleanSpec
        pExprSpec
        pFloatSpec
        pIntegerSpec
