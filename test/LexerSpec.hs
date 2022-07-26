module LexerSpec
  ( spec
  ) where

import           Lexer
import           RIO
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec                ( parse )


spec :: Spec
spec = do
  describe "integer literal" $ do
    it "should handle valid integer" $ do
      parse integerLiteral "" "42" `shouldParse` 42


  describe "float literal" $ do
    it "should handle valid float" $ do
      parse floatLiteral "" "42.0" `shouldParse` 42.0


  describe "string literal" $ do
    it "should handle valid string" $ do
      parse stringLiteral "" "\"foo bar\"" `shouldParse` "foo bar"


  describe "binary literal" $ do
    it "should handle valid binary" $ do
      parse binaryLiteral "" "0b101010111100000100100011" `shouldParse` 11256099


  describe "octal literal" $ do
    it "should handle valid octal" $ do
      parse octalLiteral "" "0o52740443" `shouldParse` 11256099


  describe "hexadecimal literal" $ do
    it "should handle valid hexadecimal" $ do
      parse hexadecimalLiteral "" "0xABC123" `shouldParse` 11256099
