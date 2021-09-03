{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Parser
import           TestHelpers


-- Values


pBooleanSpec :: Spec
pBooleanSpec = describe "Boolean" $ do
    it "should handle value True" $ shouldMatch pBoolean "True" (LBool True)

    it "should handle value False" $ shouldMatch pBoolean "False" (LBool False)


pIntegerSpec :: Spec
pIntegerSpec = describe "Integer" $ do
    it "should handle a valid integer" $ shouldMatch pInteger "42" (LInt 42)

    it "should handle a valid negative integer"
        $ shouldMatch pInteger "-42" (LInt (-42))


pFloatSpec :: Spec
pFloatSpec = describe "Float" $ do
    it "should handle a valid float" $ shouldMatch pFloat "42.0" (LFloat 42.0)

    it "should handle a valid negative float"
        $ shouldMatch pFloat "-42.0" (LFloat (-42.0))


pStringSpec :: Spec
pStringSpec = describe "String" $ do
    it "should handle a valid string"
        $ shouldMatch pString "\"hello word\"" (LStr "hello world")


pCollectionSpec :: Spec
pCollectionSpec = describe "Collection" $ do
    it "should handle a valid list of integers"
        $ shouldMatch pList "[1,2,3]" ( CList [ LInt 1, LInt 2, LInt 3 ] )
    
    it "should handle a valid list of floats"
        $ shouldMatch pList "[1.0,2.0]" ( CList [ LFloat 1.0, LFloat 2.0 ] )
    
    it "should handle a valid list of strings"
        $ shouldMatch pList "[\"hello\",\"world\"]" ( CList [ LStr "hello", LStr "world" ] )


-- Expressions


pExprSpec :: Spec
pExprSpec = describe "Expression" $ do
    it "should handle simple addition"
        $ shouldMatch pExpr "1 + 2" (Binary Sum (LInt 1) (LInt 2))

    it "should handle simple subraction"
        $ shouldMatch pExpr "1 - 2" (Binary Difference (LInt 1) (LInt 2))

    it "should handle simple division"
        $ shouldMatch pExpr "2 / 4" (Binary Quotient (LInt 2) (LInt 4))

    it "should handle simple multiplication"
        $ shouldMatch pExpr "2 * 4" (Binary Product (LInt 2) (LInt 4))

    it "should handle parens with addition" $ shouldMatch
        pExpr
        "(1 + 2) + 3"
        (Binary Sum (Binary Sum (LInt 1) (LInt 2)) (LInt 3))

    it "should handle multiplication before addtion" $ shouldMatch
        pExpr
        "1 + 2 * 3"
        (Binary Sum (LInt 1) (Binary Product (LInt 2) (LInt 3)))

    it "should handle modulus"
        $ shouldMatch pExpr "16 mod 4" (Binary Modulo (LInt 16) (LInt 4))

    it "should handle greater than"
        $ shouldMatch pExpr "4 > 2" (Binary GreaterThan (LInt 4) (LInt 2))

    it "should handle less than"
        $ shouldMatch pExpr "4 < 2" (Binary LessThan (LInt 4) (LInt 2))

    it "should handle greater than equals" $ shouldMatch
        pExpr
        "4 >= 2"
        (Binary GreaterThanEquals (LInt 4) (LInt 2))

    it "should handle less than equals" $ shouldMatch
        pExpr
        "4 <= 2"
        (Binary LessThanEquals (LInt 4) (LInt 2))
