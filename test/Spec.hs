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
    runParser parser "" input `shouldBe` Right result


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


booleanSpec :: Spec
booleanSpec =
    describe "Boolean" $ do
        it "should handle value True" $
            shouldMatch pBoolean "True" (Bool True)
        
        it "should handle value False" $
            shouldMatch pBoolean "False" (Bool False)


astSpec :: Spec
astSpec =
    describe "Integer AST" $ do
        it "should handle real number" $
            shouldMatch pInteger "3" (Int 3)
            
        -- it "should handle real number" $
        --     shouldMatch pInteger "-3" (Negation (Int 3))
        
        it "should handle simple addition" $
            shouldMatch pExpr "1 + 2" (Addition (Int 1) (Int 2))
            
        it "should handle simple subraction" $
            shouldMatch pExpr "1 - 2" (Subtraction (Int 1) (Int 2))
            
        it "should handle simple division" $
            shouldMatch pExpr "2 / 4" (Division (Int 2) (Int 4))
            
        it "should handle simple multiplication" $
            shouldMatch pExpr "2 * 4" (Product (Int 2) (Int 4))
            
        it "should handle parens with addition" $
            shouldMatch pExpr "(1 + 2) + 3" (Addition (Addition (Int 1) (Int 2)) (Int 3))
            
        it "should handle multiplication before addtion" $
            shouldMatch pExpr "1 + 2 * 3" (Addition (Int 1) (Product (Int 2) (Int 3)))


-- Main


main :: IO ()
main = 
    hspec $ do
        integerSpec
        floatSpec
        stringLiteralSpec
        astSpec
