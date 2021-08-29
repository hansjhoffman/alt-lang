# Syntax

## Primitive Types

```
Char
String
Number -> Int / Float
Boolean
List
Tuple
```

## HKT's

```
type Option<A> = None | Some<A>

type Result<E, A> = Error<E> | Ok<A>

type TaskEither<E, A> = Task<Result<E, A>>
```

## Arithmetic Operators

```
Addition -> (+) :: Number
Subtraction -> (-) :: Number
Multiplication -> (*) :: Number
Float Division -> (/) :: Int
Integer Division -> (//) :: Float
Modulus -> (mod) :: Int
```

## Relational Operators

```
GT -> (>) :: Boolean
LT -> (<) :: Boolean
LE -> (<=) :: Boolean
GE -> (>=) :: Boolean
EQ -> (==) :: Boolean
NE -> (!=) :: Boolean
```

## Values

```
<|> 1 + 1
2 :: Number

<|> 2 ^ 4
16 :: Number

<|> 16.0 / 4
4 :: Float

<|> "hello world"
"hello world" :: String

<|> "hello " ++ "world"
"hello world" :: String

<|> True
True :: Boolean

<|> True || False
True :: Boolean
```

## Functions

Functions require type annotations

```
add :: Int -> Int -> Int
add a b = 
    a + b
    

<|> add 21 21
42 :: Number
```

## If Expression

```
isEven :: a -> Boolean
isEven a =
    if a % 2 == 0 then
        True

    else
        False


<|> isEven 4
True :: Boolean
```

## Pattern Matching

```
filePath :: Option Path -> Result String
filePath path =
    case path of
        None -> 
            Error "no file path"
        
        Some p ->
            Ok p
```

## Modules

```
module HelloWorld where
    @moduledoc {- 
        Some module documentation. Helps with doc generation.
    -}
    
    import Data.Text (Text)
    import Text.Megaparsec

    alias Text.Megaparsec.Char.Lexer as L
    
    @doc {- 
        Function specific documentation. Helps with doc generation.
    -}
    index :: Connection -> RouterContext -> Connection
    index conn ( Hash hash ) =
       conn
       |> render LinkIndexView hash: hash
end
```
