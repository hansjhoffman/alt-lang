module Types where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text
