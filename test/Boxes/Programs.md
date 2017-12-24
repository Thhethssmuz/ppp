# Programs

\cleardoublepage

Plain code blocks on a new page should start aligned with the top of the page.

\newpage

```
main :: IO ()
main = do
  putStrLn "hello world"
```

\lipsum[2]

\cleardoublepage

Plain `.long` code blocks on a new page should start aligned with the top of the page.

\newpage

```{.long}
main :: IO ()
main = do
  putStrLn "hello world"
```

\lipsum[2]

\cleardoublepage

Highlighted code blocks on a new page should start aligned with the top of the page.

\newpage

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

\lipsum[2]

\cleardoublepage

Highlighted `.long` code blocks on a new page should start aligned with the top of the page.

\newpage

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: {.long}

\lipsum[2]

\cleardoublepage

Wrapping code blocks on a new page should start aligned with the top of the page.

\newpage

```
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: {.wrap}

\lipsum[2]


\cleardoublepage

Highlighted wrapping code blocks on a new page should start aligned with the top of the page.

\newpage

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: {.wrap}

\lipsum[2]



\cleardoublepage

Normal and `.long` code block appearance should be roughly equal.

```
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: {}

\lipsum[2]

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: {}

\lipsum[2]

\newpage

Normal and `.long` code block appearance should be roughly equal.

```
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: {.long}

\lipsum[2]

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: {.long}

\lipsum[2]



\cleardoublepage

Normal and `.long` code block appearance should be roughly equal.

```
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {}

\lipsum[2]

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {}

\lipsum[2]

\newpage

Normal and `.long` code block appearance should be roughly equal.

```
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {.long}

\lipsum[2]

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {.long}

\lipsum[2]



\cleardoublepage

Normal and `.long` code block appearance should be roughly equal.

```
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {style=plaintop}

\lipsum[2]

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {style=plaintop}

\lipsum[2]

\newpage

Normal and `.long` code block appearance should be roughly equal.

```
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {.long style=plaintop}

\lipsum[2]

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {.long style=plaintop}

\lipsum[2]
