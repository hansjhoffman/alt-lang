{-# LANGUAGE OverloadedStrings #-}

module LexerSpec where

import Test.Hspec

import TestHelpers
import Lexer


-- Values


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


-- binarySpec :: Spec
-- binarySpec =
--     describe "Binary" $ do
--         it "should handle valid binary" $
--             shouldMatch binary' "0b101010111100000100100011" (101010111100000100100011)


-- hexadecimalSpec :: Spec
-- hexadecimalSpec =
--     describe "Hexadecimal" $ do
--         it "should handle valid hexadecimal" $
--             shouldMatch hexadecimal' "0xABC123" (ABC123)


-- octalSpec :: Spec
-- octalSpec =
--     describe "Octal" $ do
--         it "should handle valid octal" $
--             shouldMatch octal' "0o52740443" (52740443)


-- Strings


stringLiteralSpec :: Spec
stringLiteralSpec =
    describe "String Literal" $ do
        it "should handle valid strings" $
            shouldMatch stringLiteral "\"hello world\"" "hello world"
