module Language.Alt.AST.Types where

import           Data.Void                      ( Void )
import           RIO.Text                       ( Text )
import           Text.Megaparsec                ( Parsec )


type Parser = Parsec Void Text
