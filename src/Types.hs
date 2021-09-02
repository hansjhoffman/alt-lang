module Types where
    
import Control.Applicative
import qualified Data.Text as T


newtype Parser a = Parser
    { unParser :: T.Text -> Maybe (a, T.Text)
    }

-- Functor


instance Functor Parser where
    fmap f (Parser p) =
        Parser $
            \input -> do
                (a, rest) <- p input
                Just (f a, rest)


-- Alternative


instance Applicative Parser where
    pure p =
        Parser $
            \input -> 
                Just (p, input)
    
    (Parser p1) <*> (Parser p2) =
        Parser $
            \input -> do
                (f, rest) <- p1 input
                (a, rest') <- p2 rest
                Just (f a, rest')

    p1 *> p2 =
        p2
        
    p1 <* p2 =
        p1


-- Alternative


instance Alternative Parser where
    empty =
        Parser $
            \input ->
                Nothing
    
    (Parser p1) <|> (Parser p2) =
        Parser $
            \input ->
                p1 input <|> 
                p2 input
    
    -- applied as many times as possible, but allows zero
    -- some (Parser p) =
    --     undefined
    
    -- applied as many times as possible, but at least once
    -- many (Parser p) =
    --     undefined


-- Monad


instance Monad Parser where
    (Parser p) >>= f =
        Parser $
            \input -> do
                (a, rest) <- p input
                unParser (f a) rest
    
    ma >> mb =
        mb
    
    return a =
        pure a
