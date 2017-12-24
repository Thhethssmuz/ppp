# Wrap

\cleardoublepage

Wrap figures alignment `outer`.

![](test/Test.jpg){.wrap wrap=outer}

\lipsum[2]

\newpage

Wrap figures alignment `outer`.

![](test/Test.jpg){.wrap wrap=outer}

\lipsum[2]

\cleardoublepage

Wrap figures alignment `left`.

![](test/Test.jpg){.wrap wrap=left}

\lipsum[2]

\newpage

Wrap figures alignment `left`.

![](test/Test.jpg){.wrap wrap=left}

\lipsum[2]

\cleardoublepage

Wrap figures alignment `right`.

![](test/Test.jpg){.wrap wrap=right}

\lipsum[2]

\newpage

Wrap figures alignment `right`.

![](test/Test.jpg){.wrap wrap=right}

\lipsum[2]

\cleardoublepage

Wrap figures alignment `inner`.

![](test/Test.jpg){.wrap wrap=inner}

\lipsum[2]

\newpage

Wrap figures alignment `inner`.

![](test/Test.jpg){.wrap wrap=inner}

\lipsum[2]



\cleardoublepage

Wrap captions.

![](test/Test.jpg)

Figure: Caption {.wrap}

\lipsum[2]

\newpage

Wrap captions.

![](test/Test.jpg)

Figure: Caption {.wrap style=plaintop}

\lipsum[2]



\cleardoublepage

Wraps on a new page should start aligned with the top of the page.

\newpage

![](test/Test.jpg){.wrap}

\lipsum[2]

\cleardoublepage

Wraps between paragraphs should start flush with the top of the paragraph proceeding it.

\newpage

\lipsum[2]

![](test/Test.jpg){.wrap}

\lipsum[2]



\cleardoublepage

Multi wrap.

\newpage

::: box
![](test/Test.jpg)

Figure: Left

![](test/Test.jpg)

Figure: Right
:::

Figure: Caption {.wrap}

\lipsum[2]

\cleardoublepage

Multi wrap `plaintop`.

\newpage

\lipsum[2]

::: box
![](test/Test.jpg)

Figure: Left

![](test/Test.jpg)

Figure: Right
:::

Figure: Caption {.wrap style=plaintop}

\lipsum[2]

\cleardoublepage

Multi wrap recursive `plaintop`.

\newpage

\lipsum[2]

::: box
![](test/Test.jpg)

Figure: Left {style=plaintop}

![](test/Test.jpg)

Figure: Right {style=plaintop}
:::

Figure: Caption {.wrap style=plaintop}

\lipsum[2]
