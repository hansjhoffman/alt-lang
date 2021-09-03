module TestHelpers where

import           Test.Hspec
import           Text.Megaparsec                          ( runParser )

import           Types


-- Helpers


shouldMatch :: (Show a, Eq a) => Parser a -> String -> a -> IO ()
shouldMatch parser input result =
    runParser parser "" input `shouldBe` Right result
