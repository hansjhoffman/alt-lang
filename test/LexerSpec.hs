module LexerSpec
  ( spec
  ) where

import           Lexer
import           Prelude                        ( ($)
                                                , Either(..)
                                                )
import           Test.Hspec
import           Text.Megaparsec                ( runParser )


spec :: Spec
spec = do
  describe "integer literal" $ do
    it "should handle valid integer" $ do
      let result = runParser integerLiteral "" "42"
      result `shouldBe` Right 42

    it "should handle valid signed integer" $ do
      let result = runParser signedInteger "" "-42"
      result `shouldBe` Right (-42)


  describe "float literal" $ do
    it "should handle valid float" $ do
      let result = runParser floatLiteral "" "42.0"
      result `shouldBe` Right 42.0

    it "should handle valid signed float" $ do
      let result = runParser signedFloat "" "-42.0"
      result `shouldBe` Right (-42.0)


  describe "string literal" $ do
    it "should handle valid string" $ do
      let result = runParser stringLiteral "" "\"foo bar\""
      result `shouldBe` Right "foo bar"


  describe "binary literal" $ do
    it "should handle valid binary" $ do
      let result = runParser binaryLiteral "" "0b101010111100000100100011"
      result `shouldBe` Right 11256099


  describe "octal literal" $ do
    it "should handle valid octal" $ do
      let result = runParser octalLiteral "" "0o52740443"
      result `shouldBe` Right 11256099


  describe "hexadecimal literal" $ do
    it "should handle valid hexadecimal" $ do
      let result = runParser hexadecimalLiteral "" "0xABC123"
      result `shouldBe` Right 11256099
