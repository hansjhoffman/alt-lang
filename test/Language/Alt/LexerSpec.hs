module Language.Alt.LexerSpec
  ( spec
  ) where

import           Language.Alt.Lexer
import           RIO
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec                ( parse )


spec :: Spec
spec = do
  describe "literals" $ do
    it "should handle integer" $ do
      parse numericLiteral "" "42" `shouldParse` 42

    it "should handle float" $ do
      parse numericLiteral "" "42.0" `shouldParse` 42.0

    it "should handle string" $ do
      parse stringLiteral "" "\"foo bar\"" `shouldParse` "foo bar"

    it "should handle character" $ do
      parse charLiteral "" "x" `shouldParse` 'x'

    it "should handle binary" $ do
      parse binaryLiteral "" "0b101010111100000100100011" `shouldParse` 11256099

    it "should handle octal" $ do
      parse octalLiteral "" "0o52740443" `shouldParse` 11256099

    it "should handle hexadecimal" $ do
      parse hexadecimalLiteral "" "0xABC123" `shouldParse` 11256099
