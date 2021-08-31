{-# LANGUAGE OverloadedStrings #-}

module Main where
    
import Test.Hspec (hspec)

import LexerSpec (integerSpec, floatSpec, stringLiteralSpec)
import ParserSpec (pBooleanSpec, pFloatSpec, pExprSpec, pIntegerSpec)


-- Main


main :: IO ()
main = 
    hspec $ do
        integerSpec
        floatSpec
        stringLiteralSpec
        pBooleanSpec
        pExprSpec
        pFloatSpec
        pIntegerSpec
