{-# LANGUAGE OverloadedStrings #-}

module LexerSpec where

import Test.Hspec

import TestHelpers
import Lexer


-- Tests


integerSpec :: Spec
integerSpec = 
    describe "Integer" $ do
        it "should handle valid integer with no space" $
            shouldMatch integer "42" 42
            
        it "should handle signed integers" $
            shouldMatch signedInteger "-42" (-42)
    
    
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
