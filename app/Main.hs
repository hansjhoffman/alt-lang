module Main where

import Parser
import Types


-- Helpers


prettyPrint :: Expr -> String
prettyPrint (Int a) =
    show a


-- Main


main :: IO ()
main = 
    undefined
