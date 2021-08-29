{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (runParser)

import Lexer


-- Helpers


shouldMatch :: (Show a, Eq a) => Parser a -> Text -> a -> IO ()
shouldMatch parser input result =
    runParser parser "" input `shouldBe` (Right result)


-- Tests


integerSpec :: Spec
integerSpec = 
    describe "Integer" $ do
        it "should handle valid integer with no space" $
            shouldMatch integer "42" 42
            
        it "should handle signed integers" $
            shouldMatch signedInteger "-42" (-42)
      
        -- it "should handle valid integer with space" $
        --     shouldMatch integer " 42" 42
    
    
floatSpec :: Spec
floatSpec =
    describe "Float" $ do
        it "should handle valid float with no space" $
            shouldMatch float "42.0" 42.0
            
        it "should handle signed floats" $
            shouldMatch signedFloat "-42.0" (-42.0)


stringLiteralSpec :: Spec
stringLiteralSpec =
    describe "String Literal" $ do
        it "should handle valid strings" $
            shouldMatch stringLiteral "\"hello world\"" "hello world"


-- Main


main :: IO ()
main = 
    hspec $ do
        integerSpec
        floatSpec
        stringLiteralSpec
