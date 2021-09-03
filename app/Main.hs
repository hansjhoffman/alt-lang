module Main where

import           Control.Monad.IO.Class                   ( MonadIO )
import           Text.Megaparsec                          ( errorBundlePretty
                                                          , parse
                                                          )
import           Text.Pretty.Simple                       ( CheckColorTty(..)
                                                          , OutputOptions(..)
                                                          , defaultOutputOptionsNoColor
                                                          , pPrintOpt
                                                          )

import           Lexer
import           Types


prettyPrint :: (MonadIO m, Show a) => a -> m ()
prettyPrint = pPrintOpt CheckColorTty $ defaultOutputOptionsNoColor
    { outputOptionsIndentAmount  = 2
    , outputOptionsCompact       = True
    , outputOptionsCompactParens = True
    }


runParser :: Parser a -> String -> Either String a
runParser parser code = case parse parser "" code of
    Left  err  -> Left $ errorBundlePretty err

    Right prog -> Right prog


main :: IO ()
main = undefined

