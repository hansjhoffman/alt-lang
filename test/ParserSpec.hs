{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec

import TestHelpers
import Parser


-- Tests


booleanSpec :: Spec
booleanSpec =
    describe "Boolean" $ do
        it "should handle value True" $
            shouldMatch pBoolean "True" (Boolean True)
        
        it "should handle value False" $
            shouldMatch pBoolean "False" (Boolean False)


pIntegerSpec :: Spec
pIntegerSpec =
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
