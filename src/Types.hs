module Types where

import           Data.Void
import           Text.Megaparsec                          ( Parsec )

type Parser = Parsec Void String
