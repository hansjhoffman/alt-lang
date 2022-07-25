module ParserSpec
  ( spec
  ) where

import           Parser
import           Prelude                        ( ($)
                                                , Either(..)
                                                )
import           Test.Hspec
import           Text.Megaparsec                ( runParser )


spec :: Spec
spec = do
  describe "integer literal" $ do
    it "should handle valid integer" $ do
      let result = runParser pInteger "" "42"
      result `shouldBe` Right (IntLiteral 42)

    -- it "should handle valid signed integer" $ do
    --   let result = runParser pInteger "" "-42"
    --   result `shouldBe` Right (IntLiteral (-42))

    it "should handle valid binary" $ do
      let result = runParser pBinary "" "0b101010111100000100100011"
      result `shouldBe` Right (IntLiteral 11256099)

    it "should handle valid hexadecimal" $ do
      let result = runParser pHexadecimal "" "0xABC123"
      result `shouldBe` Right (IntLiteral 11256099)

    it "should handle valid octal" $ do
      let result = runParser pOctal "" "0o52740443"
      result `shouldBe` Right (IntLiteral 11256099)


  describe "float literal" $ do
    it "should handle valid float" $ do
      let result = runParser pFloat "" "42.0"
      result `shouldBe` Right (FloatLiteral 42.0)

    -- it "should handle valid signed float" $ do
    --   let result = runParser pFloat "" "-42"
    --   result `shouldBe` Right (FloatLiteral 42.0)


  describe "string literal" $ do
    it "should handle valid string" $ do
      let result = runParser pString "" "\"foo bar\""
      result `shouldBe` Right (StringLiteral "foo bar")
