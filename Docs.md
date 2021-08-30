# Syntax

## Built In Types

```
Int (Number)
Float (Number)
Function
String = ""
Char = ''
Unit = ()
Record = {}
List<A> = []
Tuple = ()
Boolean = True | False
Maybe<A> = Nothing | Just<A>
Result<E, A> = Error<E> | Ok<A>
Task<A>
Future<E, A> = Task<Result<E, A>>
```

## Arithmetic Operators

```
Addition -> (+) :: Number
Subtraction -> (-) :: Number
Multiplication -> (*) :: Number
Float Division -> (/) :: Float
Integer Division -> (//) :: Int
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

## Boolean Operators

```
negation -> not :: Boolean
and -> and :: Boolean
or -> or :: Boolean
in collection -> in :: Boolean
not in collection -> not in :: Boolean
```

## Comments

Multiline -> `{- Some Comment -}`
Single line -> `-- Some Comment`

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

<|> 'a'
'a' :: Char

<|> "hello " ++ "world"
"hello world" :: String

<|> True
True :: Boolean

<|> True || False
True :: Boolean

<|> 0xABC123
11256099 :: Number

<|> 0o52740443
11256099 :: Number

<|> 0b101010111100000100100011
11256099 :: Number

<|> 1_000_000_000_000
1000000000000 :: Number
```

## Functions

Functions require type annotations, are lazy, have implicit returns, and only take one argument at a time.

```
add :: Int -> Int -> Int
add a b =
    a + b


<|> add 21 21
42 :: Number
```

## Anonymous Functions

```
square :: List Int
squares =
    List.map (fn n => n ^ 2 end) (List.range 1 100)
```

## If-Else Expression

allow guards ??

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

## Records

Records are immutable. Record types are nominal meaning two different record definitions with the exact same fields are not the same. Commas go at the start of the line. Access properties using the `.` operator e.g. `x.foo`

```
{ name: "Alice"
, age: 42
}
```

## Lists

Lists are immutable. Values are separated by a comma. Concat operations use the `++` operator e.g. `[1, 2, 3] ++ [4, 5, 6] = [1, 2, 3, 4, 5, 6]`

## Tuples

Tuples are immutable. A collection of heterogenous items.

## Strings

Multiline strings

```
"""
Some multine string
looks like this.
"""
```

## Type Variants

`type CustomType = Foo | Bar | Baz a`

## Let Blocks

```
let
    twentyFour :: Int
    twentyFour =
        3 * 8

    sixteen :: Int
    sixteen =
        4 ^ 2
in
twentyFour + sixteen
```

## Pattern Matching

Match against variants and ensures all cases are covered. You can use the wildcard `_` as a catch-all case.

```
filePath :: Maybe Path -> Result String
filePath path =
    match path on
        Nothing =>
            Error "no file path"

        Just p =>
            Ok p
```

## Modules

Modules are a way to group types and functions together. Each type definition functions automatically become accessible by other modules.

```
module Hello.World where
    @moduledoc """
        Some module documentation. Helps with doc generation.
    """

    -- open imports
    import Data.Text (Text)
    import Text.Megaparsec

    -- qualified imports
    alias Text.Megaparsec.Char.Lexer as L

    @doc """
        Function specific documentation. Helps with doc generation.
    """
    index :: Connection -> RouterContext -> Connection
    index conn ( Hash hash ) =
       conn
       |> render LinkIndexView hash: hash
end
```

## Spacing

Spaces (4), not tabs. 2 lines between functions and types. 1 between everything else.

## Frameworks

- Rax -> web framework similar to Elixir's Phoenix
- Something similar to Elixir's Ecto
- Formatter like Go & Elm
- CLI tool like Elixir's mix
