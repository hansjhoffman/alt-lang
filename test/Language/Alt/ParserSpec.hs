module Language.Alt.ParserSpec
  ( spec
  ) where

import           Language.Alt.Parser
import           RIO
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec                ( parse )


spec :: Spec
spec = do
  describe "integer literal" $ do
    it "should handle valid integer" $ do
      parse pInteger "" "42" `shouldParse` Value (IntLiteral 42)

    it "should handle valid binary" $ do
      parse pBinary "" "0b101010111100000100100011" `shouldParse` Value (IntLiteral 11256099)

    it "should handle valid hexadecimal" $ do
      parse pHexadecimal "" "0xABC123" `shouldParse` Value (IntLiteral 11256099)

    it "should handle valid octal" $ do
      parse pOctal "" "0o52740443" `shouldParse` Value (IntLiteral 11256099)


  describe "float literal" $ do
    it "should handle valid float" $ do
      parse pFloat "" "42.0" `shouldParse` Value (FloatLiteral 42.0)


  describe "string literal" $ do
    it "should handle valid string" $ do
      parse pString "" "\"foo bar\"" `shouldParse` Value (StringLiteral "foo bar")


  describe "character literal" $ do
    it "should handle valid character" $ do
      parse pChar "" "x" `shouldParse` Value (CharLiteral 'x')


  describe "negation" $ do
    it "should handle integer negation" $ do
      parse pExpr "" "-42" `shouldParse` Negate (Value (IntLiteral 42))

    -- it "should handle float negation" $ do
    --   parse pExpr "" "-42.0" `shouldParse` Negation (FloatLiteral 42.0)


  describe "sum" $ do
    it "should handle integer addition" $ do
      parse pExpr "" "42 + 42"
        `shouldParse` ABinary Add (Value (IntLiteral 42)) (Value (IntLiteral 42))

    -- it "should handle float addition" $ do
    --   parse pExpr "" "42.0 + 42.0" `shouldParse` Sum (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "subtact" $ do
    it "should handle integer subtraction" $ do
      parse pExpr "" "42 - 42"
        `shouldParse` ABinary Subtract (Value (IntLiteral 42)) (Value (IntLiteral 42))

    -- it "should handle float subtraction" $ do
    --   parse pExpr "" "42.0 - 42.0" `shouldParse` Subtract (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "product" $ do
    it "should handle integer multiplication" $ do
      parse pExpr "" "42 * 42"
        `shouldParse` ABinary Multiply (Value (IntLiteral 42)) (Value (IntLiteral 42))

    -- it "should handle float multiplication" $ do
    --   parse pExpr "" "42.0 * 42.0" `shouldParse` Product (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "division" $ do
    it "should handle integer division" $ do
      parse pExpr "" "42 / 42"
        `shouldParse` ABinary Divide (Value (IntLiteral 42)) (Value (IntLiteral 42))

    -- it "should handle float division" $ do
    --   parse pExpr "" "42.0 / 42.0" `shouldParse` Division (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "modulo" $ do
    it "should handle modulo remainder" $ do
      parse pExpr "" "42 % 2"
        `shouldParse` ABinary Modulus (Value (IntLiteral 42)) (Value (IntLiteral 2))


  describe "exponent" $ do
    it "should handle power" $ do
      parse pExpr "" "42 ^ 2"
        `shouldParse` ABinary Exponent (Value (IntLiteral 42)) (Value (IntLiteral 2))


  describe "boolean" $ do
    it "should handle True" $ do
      parse pExpr "" "True" `shouldParse` Value (BooleanLiteral True)

    it "should handle False" $ do
      parse pExpr "" "False" `shouldParse` Value (BooleanLiteral False)


  describe "relation" $ do
    it "should handle '=='" $ do
      parse pExpr "" "2 == 3"
        `shouldParse` RBinary EqualTo (Value (IntLiteral 2)) (Value (IntLiteral 3))

    it "should handle '/='" $ do
      parse pExpr "" "2 /= 3"
        `shouldParse` RBinary NotEqualTo (Value (IntLiteral 2)) (Value (IntLiteral 3))

    it "should handle '<='" $ do
      parse pExpr "" "2 <= 3"
        `shouldParse` RBinary LessThanOrEqualTo (Value (IntLiteral 2)) (Value (IntLiteral 3))

    it "should handle '>='" $ do
      parse pExpr "" "2 >= 3"
        `shouldParse` RBinary GreaterThanOrEqualTo (Value (IntLiteral 2)) (Value (IntLiteral 3))

    it "should handle '>'" $ do
      parse pExpr "" "2 > 3"
        `shouldParse` RBinary GreaterThan (Value (IntLiteral 2)) (Value (IntLiteral 3))

    it "should handle '<'" $ do
      parse pExpr "" "2 < 3"
        `shouldParse` RBinary LessThan (Value (IntLiteral 2)) (Value (IntLiteral 3))


  describe "logical" $ do
    it "should handle 'and'" $ do
      parse pExpr "" "True and False"
        `shouldParse` LBinary And (Value (BooleanLiteral True)) (Value (BooleanLiteral False))

    it "should handle 'or'" $ do
      parse pExpr "" "True or False"
        `shouldParse` LBinary Or (Value (BooleanLiteral True)) (Value (BooleanLiteral False))

    it "should handle 'xor'" $ do
      parse pExpr "" "True xor False"
        `shouldParse` LBinary Xor (Value (BooleanLiteral True)) (Value (BooleanLiteral False))

    it "should handle 'not'" $ do
      parse pExpr "" "not True" `shouldParse` Not (Value (BooleanLiteral True))
