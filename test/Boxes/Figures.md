# Figures

\cleardoublepage

Figures on a new page should start aligned with the top of the page.

\newpage

![](test/Test.jpg){width=50%}

\lipsum[2]

\cleardoublepage

Wrap figures on a new page should start aligned with the top of the page.

\newpage

![](test/Test.jpg){.wrap}

\lipsum[2]

\cleardoublepage

Unnecessary double box wrapping, with and without mixed environments.

\newpage

:::{.box width=50%}
![](test/Test.jpg)
:::

\lipsum[2]

:::{.box width=50%}
![](test/Test.jpg)

Figure: {width=100%}
:::

\lipsum[2]

\cleardoublepage

Unnecessary triple box wrapping, with and without mixed environments.

\newpage

:::{.box width=50%}
:::: box
![](test/Test.jpg)
::::
:::


\lipsum[2]

:::{.box width=50% box-group=misc}
::::{.box box-group=table}
![](test/Test.jpg)

Figure: {width=100%}
::::
:::

\lipsum[2]


\cleardoublepage

Figure alignment.

\newpage

![](test/Test.jpg){align="top left" width=50%}

![](test/Test.jpg){align="top centre" width=50%}

![](test/Test.jpg){align="top right" width=50%}
