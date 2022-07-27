module ParserSpec
  ( spec
  ) where

import           Parser
import           RIO
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec                ( parse )


spec :: Spec
spec = do
  describe "integer literal" $ do
    it "should handle valid integer" $ do
      parse pInteger "" "42" `shouldParse` IntLiteral 42

    it "should handle valid binary" $ do
      parse pBinary "" "0b101010111100000100100011" `shouldParse` IntLiteral 11256099

    it "should handle valid hexadecimal" $ do
      parse pHexadecimal "" "0xABC123" `shouldParse` IntLiteral 11256099

    it "should handle valid octal" $ do
      parse pOctal "" "0o52740443" `shouldParse` IntLiteral 11256099


  describe "float literal" $ do
    it "should handle valid float" $ do
      parse pFloat "" "42.0" `shouldParse` FloatLiteral 42.0


  describe "string literal" $ do
    it "should handle valid string" $ do
      parse pString "" "\"foo bar\"" `shouldParse` StringLiteral "foo bar"


  describe "negation" $ do
    it "should handle integer negation" $ do
      parse pExpr "" "-42" `shouldParse` Negate (IntLiteral 42)

    -- it "should handle float negation" $ do
    --   parse pExpr "" "-42.0" `shouldParse` Negation (FloatLiteral 42.0)


  describe "sum" $ do
    it "should handle integer addition" $ do
      parse pExpr "" "42 + 42" `shouldParse` ABinary Add (IntLiteral 42) (IntLiteral 42)

    -- it "should handle float addition" $ do
    --   parse pExpr "" "42.0 + 42.0" `shouldParse` Sum (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "subtact" $ do
    it "should handle integer subtraction" $ do
      parse pExpr "" "42 - 42" `shouldParse` ABinary Subtract (IntLiteral 42) (IntLiteral 42)

    -- it "should handle float subtraction" $ do
    --   parse pExpr "" "42.0 - 42.0" `shouldParse` Subtract (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "product" $ do
    it "should handle integer multiplication" $ do
      parse pExpr "" "42 * 42" `shouldParse` ABinary Multiply (IntLiteral 42) (IntLiteral 42)

    -- it "should handle float multiplication" $ do
    --   parse pExpr "" "42.0 * 42.0" `shouldParse` Product (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "division" $ do
    it "should handle integer division" $ do
      parse pExpr "" "42 / 42" `shouldParse` ABinary Divide (IntLiteral 42) (IntLiteral 42)

    -- it "should handle float division" $ do
    --   parse pExpr "" "42.0 / 42.0" `shouldParse` Division (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "modulo" $ do
    it "should handle modulo remainder" $ do
      parse pExpr "" "42 % 2" `shouldParse` ABinary Modulo (IntLiteral 42) (IntLiteral 2)


  describe "exponent" $ do
    it "should handle power" $ do
      parse pExpr "" "42 ^ 2" `shouldParse` ABinary Exponent (IntLiteral 42) (IntLiteral 2)


  describe "boolean" $ do
    it "should handle True" $ do
      parse pExpr "" "True" `shouldParse` BooleanLiteral True

    it "should handle False" $ do
      parse pExpr "" "False" `shouldParse` BooleanLiteral False
