# Tables

\cleardoublepage

Tables on a new page should start aligned with the top of the page.

\newpage

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

\lipsum[2]

\cleardoublepage

`.long` tables on a new page should start aligned with the top of the page.

\newpage

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: {.long}

\lipsum[2]

\cleardoublepage

Wrap tables on a new page should start aligned with the top of the page.

\newpage

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: {.wrap}

\lipsum[2]




\cleardoublepage

Normal and `.long` table appearance should be roughly equal.

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: {}

\lipsum[2]

\newpage

Normal and `.long` table appearance should be roughly equal.

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: {.long}

\lipsum[2]

\cleardoublepage

Normal and `.long` table appearance should be roughly equal.

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: Caption {}

\lipsum[2]

\newpage

Normal and `.long` table appearance should be roughly equal.

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: Caption {.long}

\lipsum[2]

\cleardoublepage

Normal and `.long` table appearance should be roughly equal.

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: Caption {style="plaintop"}

\lipsum[2]

\newpage

Normal and `.long` table appearance should be roughly equal.

| **A** | **B** | **C** |
|:------|:------|:------|
| 1     | a     | A     |
| 2     | b     | B     |

Table: Caption {.long style="plaintop"}

\lipsum[2]

\cleardoublepage

Normal and `.long` table appearance should be roughly equal.

| **A** | **Long content** |
|:------|:-----------------|
| 1     | \lipsum[1]       |
| 2     | \lipsum[2]       |

Table: Caption {.normalise column-widths="10% 90%" style="plaintop" width=100%}

\lipsum[2]

\newpage

Normal and `.long` table appearance should be roughly equal.

| **A** | **Long Content** |
|:------|:-----------------|
| 1     | \lipsum[1]       |
| 2     | \lipsum[2]       |

Table: Caption {.long.normalise column-widths="10% 90%" style="plaintop" width=100%}

\lipsum[2]

\cleardoublepage

Normal and `.long` table alignment should be roughly equal.

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {align="top left"}

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {align="top centre"}

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {align="top right"}

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {.normalise align="top left" width=50%}

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {.normalise align="top centre" width=50%}

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {.normalise align="top right" width=50%}

\newpage

Normal and `.long` table alignment should be roughly equal.

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {.long align="top left"}

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {.long align="top centre"}

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {.long align="top right"}

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {.long.normalise align="top left" width=50%}

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {.long.normalise align="top centre" width=50%}

 **A**   **B**   **C**
------- ------- -------
   1      a        A
   2      b        B

Table: {.long.normalise align="top right" width=50%}
