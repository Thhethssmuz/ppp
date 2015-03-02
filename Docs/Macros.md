# Macros

Most notably ppp adds preprocessing macros for including other files and to set document metadata. Macros begin with the character `%` and must start on a new line, followed by the macro name. Macros taking an argument must be followed by a `:` and its argument and for macros taking list of arguments the arguments are separated by either `;` or a newline.

    % MacroName : Argument 1; Argument 2
                  Argument 3

All macro names are case insensitive and ignore leading and trailing spaces.

## Main

`Include`
:   Used to include a separate file into the markdown document. This macro accepts a list of arguments and includes all files in order at the point of entry in the markdown document.

`Type`
:   The document type is used to select which document template is used for the document. The `Type` macro may only occur once in a document and determines what macros and other markdown extensions are available in the rest of the document.

:   `Report`
    :   (Default) ...

:   `Article`
    :   A condensed version of the report document type.

## Language

`Language`
:   Sets the language of the document, controls the names of the generated chapter headings, caption markers and terms in the bibliography. Currently supported languages: `English` (Default), `British`, `American`, `Norsk`, `Nynorsk`

`Def`
:   Redefine a list of language terms. The syntax is of the form:

:   `% Def: termname  = New Definition`

## Page Layout

`Page`
:   ...

:   - OneSide (Default)
    - TwoSide

`PageCols`
:   Sets the number of columns of the document, default is `1`.

`PageSize`
:   Sets the size of the document page, values are specified in ISO paper sizes, i.e.: A0, A1, A2, A3, A4, etc. or B0, B1, B2, etc.. Default is `A4`.

`PageDiv`
:   Default is `10`.

`PageBcor`
:   Sets the binding correction for the document. Default is `0mm`.


## Fonts

`FontSize`
:   Sets the font size of the document. Default is `12pt`.

`FontMain`
:   Sets the main font of the document, the font must be available on the system. Default is `Open Sans`.

`FontSans`
:   Sets the sans font of the document, the font must be available on the system. Default is `Open Sans`.

`FontMono`
:   Sets the monospace font for the document. Used by verbatim text and code blocs. The font must be available on the system. Default is `Ubuntu Mono`.

`FontMath`
:   Sets the math font for the document. Used by formulas and equations encapsulated in `$` or `$$`. The font must be available on the system. Default is selected by \LaTeX and usually is either `Computer Modern` or `Latin Modern Math`.


## Header and Footer

`Header`
:   The header for document pages. Takes a list of 1, 2 or 3 arguments. Depending on the number of arguments the header are positioned as follows:

:   1. Centre
    2. Left & Right
    3. Left & Centre & Right

:   If a setup other than the above is required, for example a single left entry, an empty dud argument of `-` can be inserted to fill the empty slot.

:   On two sided documents the order is reversed for even pages, such that left equates to the outer margin of the page.

:   If no header is defined the separating horizontal line is also removed from the page. The header is empty by default.

`Footer`
:   Sets the footer of the document. The footer macro takes a list of arguments in the same manner as described in the header macro.

:   Default the footer put page numbers at the centre of the page on one sided documents, and at the outer margins for two sided documents.

:   If overridden the page numbers may be manually included using the latex function `\pagemark`. Fore a more fancy pagination, for example of the style _Page X of X_, one can achieve this using: `Page \pagemark of \lastpage`. These latex functions may also be used in the header if desired.

## Title Page

`Titlehead`
:   Sets the header of the title page. The title head macro takes a list of arguments in the same manner as described in header macro.

`Subject`
:   Sets the subject of the document.

`Title`
:   Sets the title of the document.

`Subtitle`
:   Sets the subtitle of the document.

`Author`
:   Sets the author(s) of the document. The author macro takes a list arguments.

`Date`
:   Sets the date of the document. The latex function `\today` may be of use here.

`Publisher`
:   Sets the publisher of the document.

`Keywords`
:   Sets the keywords of the document. The keywords macro takes a list of arguments.

`Abstract`
:   Sets the abstract of the document.

`Dedication`
:   TBI

## Listings

The listing macros are all inline macros that places the desired list at the point of entry in the document. Only one of each may be present in a document.

`TableOfContents`
:   Generate a table of content.

`ListOfFigures`
:   Generate a list of figures, all images possessing a caption will be included here.

`ListOfTables`
:   Generate a list of tables, all tables possessing a caption will be included here.

`ListOfPrograms`
:   Generate a list of programs, all code blocks possessing a caption will be included here.

`ListOfExamples`
:   Generate a list of examples.


## Sectioning

Documents are divided into 4 main parts:

`Frontmatter`
:   The front matter does not number section headings. Every document starts out in this mode so there is no macro called `Frontmatter`.

`Mainmatter`
:   This part numbers section headings. The `Mainmatter` macro may only occur once in a document, and once activated it cannot be deactivated by any other means than to transition to either the appendix or back matter.

`Appendix`
:   This part changes section numbering to alphabetic and prefixes level 1 headings with the language dependant variant of _Appendices_. Under this style all level 1 headings are treated as separate appendices. The `Appendix` macro may only occur once in a document, and once activated it cannot be deactivated by any other means than a transition to the back matter.

:   The `Appendix` macro may also be used to include external files similar to the `Include` macro. If doing so the `Appendix` part of the document is activated before the first included document.

`Backmatter`
:   This style does not number section headings. The `Backmatter` macro may only occur once in a document and once activated it lasts through the rest of the document.

## Numbering

`TocDepth`
:   Sets the depth to which headings are included in the table of contents. The `TocDepth` macro is an inline function and may be used to selectively change parts of the document. Default behaviour is to not include anything before the toc and everything after the toc down to a default header level of 3. If the `TocDepth` macro appear before the `TableOfContents` macro then the toc will not modify the already set depth otherwise the `TableOfContent` macro will set the depth to 3.

`NumDepth`
:   Sets the depth to which headings are numbered in the document. The `NumDepth` macro is an inline function and may be used to selectively change parts of the document. Only valid under the main matter or appendix part of the document. If the macro occurs before the main matter the depth is applied once the main matter is enabled. If it is applied in the back matter the macro is ignored.

`BmkDepth`
:   Sets the depth to which headings are included in the pdf bookmarks. The `BmkDepth` macro is a global macro and may only occur once in the document. Default depth is `3`.

## Notes

`LinksAsNotes`
:   Changes all links to footnotes, useful for print.

`Notes`
:   Converts all footnotes to endnotes and prints the resulting list at the macros point of entry in the markdown document.

## Bibliography

`Bibliography`
:   The `Bibliography` macro uses pandoc-citeproc to generate a list of bibliographic entries from a database like file. The macro takes a list of arguments, each a bibliographic database file, and prints the bibliography at the point of entry in the markdown file. No header is generated for this list so you may find it useful to precede the macro with a sensible heading.

`CSL`
:   The `CSL` macro is used to select what style the bibliography is printed in. The macro takes one argument and it must be a valid .csl file. Default stile is IEEE.
