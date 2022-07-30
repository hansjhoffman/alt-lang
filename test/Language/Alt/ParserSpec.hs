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


  describe "negate operator" $ do
    it "should handle integer negation" $ do
      parse pExpr "" "-42" `shouldParse` Unary Negate (Value (IntLiteral 42))

    -- it "should handle float negation" $ do
    --   parse pExpr "" "-42.0" `shouldParse` Negation (FloatLiteral 42.0)


  describe "add operator" $ do
    it "should handle integer addition" $ do
      parse pExpr "" "42 + 42"
        `shouldParse` Binary Add (Value (IntLiteral 42)) (Value (IntLiteral 42))

    -- it "should handle float addition" $ do
    --   parse pExpr "" "42.0 + 42.0" `shouldParse` Sum (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "subtact operator" $ do
    it "should handle integer subtraction" $ do
      parse pExpr "" "42 - 42"
        `shouldParse` Binary Subtract (Value (IntLiteral 42)) (Value (IntLiteral 42))

    -- it "should handle float subtraction" $ do
    --   parse pExpr "" "42.0 - 42.0" `shouldParse` Subtract (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "product operator" $ do
    it "should handle integer multiplication" $ do
      parse pExpr "" "42 * 42"
        `shouldParse` Binary Multiply (Value (IntLiteral 42)) (Value (IntLiteral 42))

    -- it "should handle float multiplication" $ do
    --   parse pExpr "" "42.0 * 42.0" `shouldParse` Product (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "division operator" $ do
    it "should handle integer division" $ do
      parse pExpr "" "42 / 42"
        `shouldParse` Binary Divide (Value (IntLiteral 42)) (Value (IntLiteral 42))

    -- it "should handle float division" $ do
    --   parse pExpr "" "42.0 / 42.0" `shouldParse` Division (FloatLiteral 42.0) (FloatLiteral 42.0)


  describe "sequence/range opeator" $ do
    it "should handle power" $ do
      parse pExpr "" "1..10"
        `shouldParse` Binary Sequence (Value (IntLiteral 1)) (Value (IntLiteral 10))


  describe "boolean" $ do
    it "should handle True" $ do
      parse pExpr "" "True" `shouldParse` Value (BooleanLiteral True)

    it "should handle False" $ do
      parse pExpr "" "False" `shouldParse` Value (BooleanLiteral False)


  describe "relation" $ do
    it "should handle '=='" $ do
      parse pExpr "" "2 == 3"
        `shouldParse` Binary EqualTo (Value (IntLiteral 2)) (Value (IntLiteral 3))

    it "should handle '/='" $ do
      parse pExpr "" "2 /= 3"
        `shouldParse` Binary NotEqualTo (Value (IntLiteral 2)) (Value (IntLiteral 3))

    it "should handle '<='" $ do
      parse pExpr "" "2 <= 3"
        `shouldParse` Binary LessThanOrEqualTo (Value (IntLiteral 2)) (Value (IntLiteral 3))

    it "should handle '>='" $ do
      parse pExpr "" "2 >= 3"
        `shouldParse` Binary GreaterThanOrEqualTo (Value (IntLiteral 2)) (Value (IntLiteral 3))

    it "should handle '>'" $ do
      parse pExpr "" "2 > 3"
        `shouldParse` Binary GreaterThan (Value (IntLiteral 2)) (Value (IntLiteral 3))

    it "should handle '<'" $ do
      parse pExpr "" "2 < 3"
        `shouldParse` Binary LessThan (Value (IntLiteral 2)) (Value (IntLiteral 3))


  describe "logical" $ do
    it "should handle 'and'" $ do
      parse pExpr "" "True and False"
        `shouldParse` Binary And (Value (BooleanLiteral True)) (Value (BooleanLiteral False))

    it "should handle 'or'" $ do
      parse pExpr "" "True or False"
        `shouldParse` Binary Or (Value (BooleanLiteral True)) (Value (BooleanLiteral False))

    it "should handle 'xor'" $ do
      parse pExpr "" "True xor False"
        `shouldParse` Binary Xor (Value (BooleanLiteral True)) (Value (BooleanLiteral False))

    it "should handle 'not'" $ do
      parse pExpr "" "not True" `shouldParse` Unary Not (Value (BooleanLiteral True))
