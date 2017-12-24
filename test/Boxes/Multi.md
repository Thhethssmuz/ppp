# Multi

\cleardoublepage

Multi float, with and without mixed environments.

\newpage

::: box
![](test/Test.jpg)

![](test/Test.jpg)
:::

\lipsum[2]

::: box
![](test/Test.jpg)

Figure: {width=50%}

![](test/Test.jpg)

Figure: {width=50%}
:::

\lipsum[2]

\cleardoublepage

Multi float, with and without mixed environments.

\newpage

::: box
![](test/Test.jpg)

Figure: Left

![](test/Test.jpg)

Figure: Right
:::
Figure: {}

\lipsum[2]

::: box
![](test/Test.jpg)

Figure: Left {width=50%}

![](test/Test.jpg)

Figure: Right {width=50%}
:::

\lipsum[2]



\cleardoublepage

Mixed environments.

\newpage

::: box
![](test/Test.jpg)

Figure: Caption

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: Caption {.normalise}

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption
:::

\lipsum[2]

::: box
![](test/Test.jpg)

Figure: Caption {align="bottom centre"}

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: Caption {.normalise align="bottom centre"}

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {align="bottom centre"}
:::

\lipsum[2]

\cleardoublepage

Mixed environments with `plaintop`.

\newpage

::: box
![](test/Test.jpg)

Figure: Caption {style=plaintop}

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: Caption {.normalise style=plaintop}

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {style=plaintop}
:::

\lipsum[2]

::: box
![](test/Test.jpg)

Figure: Caption {style=plaintop align="bottom centre"}

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: Caption {.normalise style=plaintop align="bottom centre"}

```haskell
main :: IO ()
main = do
  putStrLn "hello world"
```

Program: Caption {style=plaintop align="bottom centre"}
:::

\lipsum[2]



\cleardoublepage

Sub captions.

\newpage

::: box
![](test/Test.jpg)

Figure: Left

![](test/Test.jpg)

Figure: Centre

![](test/Test.jpg)

Figure: Right
:::

Figure: Caption

\lipsum[2]

\cleardoublepage

Sub captions with `plaintop`.

\newpage

::: box
![](test/Test.jpg)

Figure: Left

![](test/Test.jpg)

Figure: Centre

![](test/Test.jpg)

Figure: Right
:::

Figure: Caption {style=plaintop}

\lipsum[2]

\cleardoublepage

Sub captions with recursive `plaintop`.

\newpage

::: box
![](test/Test.jpg)

Figure: Left {style=plaintop}

![](test/Test.jpg)

Figure: Centre {style=plaintop}

![](test/Test.jpg)

Figure: Right {style=plaintop}
:::

Figure: Caption {style=plaintop}

\lipsum[2]
