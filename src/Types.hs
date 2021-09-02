module Types where
    
import Control.Applicative
import qualified Data.Text as T


newtype Parser a = Parser
    { unParser :: T.Text -> Maybe (a, T.Text)
    }

-- Functor


instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (a, input') <- p input
            Just (f a, input')


-- Alternative


instance Applicative Parser where
    pure p =
        Parser $ \input -> 
            Just (p, input)
    
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (f, input') <- p1 input
            (a, input'') <- p2 input'
            Just (f a, input'')

    p1 *> p2 =
        p2
        
    p1 <* p2 =
        p1


-- Alternative


instance Alternative Parser where
    empty =
        Parser $ \input ->
            Nothing
    
    (Parser p1) <|> (Parser p2) =
        Parser $ \input ->
            p1 input <|> p2 input
    
    -- applied as many times as possible, but allows zero
    -- some (Parser p) =
    --     undefined
    
    -- applied as many times as possible, but at least once
    -- many (Parser p) =
    --     undefined


-- Monad


instance Monad Parser where
    (Parser p) >>= f = do
        Parser $ \input -> do
            (a, input') <- p input
            unParser (f a) input'
    
    ma >> mb =
        mb
    
    return a =
        pure a
