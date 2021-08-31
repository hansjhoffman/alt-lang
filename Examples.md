## Lists

1. Find the last element of a list.

```
tail :: [a] -> Maybe a
tail xs =
    match xs on
        [] =>
            None

        [x] =>
            Some x

        [_:xs] =>
            tail xs

tail' :: [a] -> Maybe a
tail' xs =
    xs
    |> List.reverse
    |> List.head
```

2. Find the number of elements in a list.

```
length :: [a] -> Int
length xs =
    match xs on
        [] =>
            0
            
        [_:xs] =>
            1 + length xs
```

3. Fibonacci value at n 

```
fib :: Int -> Int
fib n =
    if n < 2 then
        n

    else
        fib (n - 1) + fib (n - 2)
```

## Rax

Official web framework for Alt. Inspiration from Elixir's Phoenix Framework and IHP.

```
├── .gitignore
├── assets
│   ├── css
│   │   └── app.css
│   ├── js
│   │   └── app.js
│   ├── package.json
│   ├── static
│   │   ├── favicon.ico
│   │   ├── images
│   │   │   └── alt.png
│   │   ├── manifest.json
│   │   └── robots.txt
│   └── vendor
├── config
│   ├── Config.alt
│   ├── Dev.alt
│   ├── Prod.alt
│   └── Test.alt
├── default.nix
├── priv
│   └── Repo
│       └── Migrations
│           └── Seeds.alt
├── src
│   ├── Core
│   └── Web
│       ├── Controllers
│       │   └── PageController.alt
│       ├── Endpoint.alt
│       ├── Router.alt
│       ├── Templates
│       │   ├── Layout
│       │   │   └── app.html.alt
│       │   └── Page
│       │       └── index.html.alt
│       └── Views
│           └── ErrorView.alt
└── test
    ├── Core
    └── Web
```
