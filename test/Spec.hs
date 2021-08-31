{-# LANGUAGE OverloadedStrings #-}

module Main where
    
import Test.Hspec (hspec)

import LexerSpec (integerSpec, floatSpec, stringLiteralSpec)
import ParserSpec (booleanSpec, pIntegerSpec)


-- Main


main :: IO ()
main = 
    hspec $ do
        integerSpec
        floatSpec
        stringLiteralSpec
        booleanSpec
        pIntegerSpec
