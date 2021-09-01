{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec

import TestHelpers
import Parser


-- Values


pBooleanSpec :: Spec
pBooleanSpec =
    describe "Boolean" $ do
        it "should handle value True" $
            shouldMatch pBoolean "True" (Boolean True)
        
        it "should handle value False" $
            shouldMatch pBoolean "False" (Boolean False)


pIntegerSpec :: Spec
pIntegerSpec =
    describe "Integer" $ do
        it "should handle a valid integer" $
            shouldMatch pInteger "42" (Int 42)
            
        -- it "should handle real number" $
        --     shouldMatch pInteger "-42" (Negation (Int 42))


pFloatSpec :: Spec
pFloatSpec =
    describe "Float" $ do
        it "should handle a valid float" $
            shouldMatch pFloat "42.0" (Float 42.0)
            
        -- it "should handle real number" $
        --     shouldMatch pFloat "-42.0" (Negation (Float 42.0))


pStringSpec :: Spec
pStringSpec =
    describe "String" $ do
        it "should handle a valid string" $
            shouldMatch pString "\"hello word\"" (String "hello world")


-- Expressions


pExprSpec :: Spec
pExprSpec =
    describe "Expression" $ do
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
