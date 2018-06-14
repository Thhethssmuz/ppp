# Other

\cleardoublepage

Plain random content boxes on a new page should start aligned with the top of the page.

\newpage

::: box
TEST
:::

\lipsum[2]

\cleardoublepage

Wrapping random content on a new page should start aligned with the top of the page.

\newpage

::: box
TEST
:::

Other: {.wrap }

\lipsum[2]

\cleardoublepage

Random content captions.

\newpage

::: box
TEST
:::

Other: Caption

\lipsum[1]

::: box
TEST
:::

Other: Caption {style="plaintop"}

\lipsum[2]

\cleardoublepage

Random content alignment.

\newpage

::: box
TEST
:::

Other: {align="top left"}

::: box
TEST
:::

Other: {align="top center"}

::: box
TEST
:::

Other: {align="top right"}

::: box
TEST
:::

Other: {align="top left" width="50%"}

::: box
TEST
:::

Other: {align="top center" width="50%"}

::: box
TEST
:::

Other: {align="top right" width="50%"}

\lipsum[2]
