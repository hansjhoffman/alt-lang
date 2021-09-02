module Parser 
    ( Expr(..)
    , pInt
    ) where

import qualified Lexer as L
import Types


data Expr
    = Boolean Bool
    | Chr Char
    | Str String
    | Int Int
    | Float Float
    | List [Expr]
    | Negate Expr
    | Unit
    deriving (Eq, Show)


-- Values


pBoolean :: Parser Expr
pBoolean =
    undefined
    -- (L.string "True" $> Boolean True) <|> 
    -- (L.string "False" $> Boolean False)


pChar :: Parser Expr
pChar =
    undefined


pString :: Parser Expr
pString =
    undefined


pInt :: String -> Parser Expr
pInt input =
    Parser $
        \input -> do
            token <- L.integer input
            pure (Int token, "")


pFloat :: Parser Expr
pFloat =
    undefined


pList :: Parser Expr
pList =
    undefined


pNegate :: Parser Expr
pNegate =
    undefined


pUnit :: Parser Expr
pUnit =
    undefined
