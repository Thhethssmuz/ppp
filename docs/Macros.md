# Macros

ppp preprocessing macros are used to include other files, change the flow of the document and set document metadata. Macros begin with the character `%` and must start on a new line, followed by the macro name. Macros taking an argument must be followed by a `:` and its argument and for macros taking multiple, or lists of, arguments the arguments are separated by either `;` or a newline.

    % MacroName : Argument 1; Argument 2
                  Argument 3

All macro names are case insensitive and ignore leading and trailing spaces.

## Main

- `Include <filepath>...`: Used to include a separate file into the markdown document. This macro accepts a list of arguments and includes all files in order at the point of entry in the markdown document.

- `Type <type>`: The document type is used to select which LaTeX document-class is used for the document. The `Type` macro may only occur once in a document.

  - `Article`
  - `Report`
  - `Book`

## Page Layout

- `PageSides <number>`: Define weather or not the document should be one-sided or two-sided. Defaults to `1`, meaning the document is one-sided.

- `PageSize <paper-size>`: Sets the size of the document page, values are specified in ISO paper sizes, i.e.: `A0`, `A1`, `A2`, `A4`, etc.. or similarly for `B`-series paper sizes. Defaults to `A4`.

- `PageOrientation <orientation>`: Set the orientation of the document, i.e.: `Portrait` or `Landscape`. Defaults to `Portrait`

- `PageColumns <number> [<size>] [<balance>]`: Define the number of columns for the document. Defaults to `1`. This macro may appear multiple times in a document, each time modifying all the text following the macro or until the macro appears again.

  The optional size argument describes the separator distance between columns. Defaults to `\intextsep` (`14pt`) if not previously set by a previous invocation of the `PageColumns` macro.

  The optional balance argument determines whether or not the columns should be balanced. Valid arguments are `Balanced` and `Unbalanced`. Defaults to `Balanced` if not previously set by a previous invocation of the `PageColumns` macro.

  Only the column number argument is required, and the order of arguments does not matter.

- `PageDiv <number>`: Page division for number for calculating margins. Defaults to `10`.

- `PageBcor <size>`: Set the binding correction of the document. Defaults to `0mm`.

## Fonts

The font macros define the fonts and size used in the document. Note that these fonts must be available on the system otherwise the document will fail to render. All the font macros are global and may only appear once in a document.

- `FontSize <size>`: Defaults to `12pt`.

- `FontMain <font>`: Sets the main font of the document, the font must be available on the system.

- `FontSans <font>`: Sets the sans font of the document, the font must be available on the system.

- `FontMono <font>`: Sets the monospace font for the document. Used by verbatim text and code blocs. The font must be available on the system.

- `FontMath <font>`: Sets the math font for the document. Used by formulas and equations encapsulated in `$`'s or `$$`'s.

## Header and Footer

The header and footer macros define the header and footer of the document. Both macros take a list of 0, 1, 2 or 3 arguments, and depending on the number of arguments the header and/or footer are positioned as follows:

0. No header/footer
1. Centre
2. Left & Right
3. Left & Centre & Right

If a setup other than the above is required, for example a single left entry, a dummy argument of `-` can be inserted to fill the empty slot(s). On two sided documents the order is reversed for even pages, such that left equates to the outer margin of the page. For two sided documents you may alternatively pass 4 or 6 arguments to specify different configurations for odd and even pages.

4. Odd Outer & Odd Inner & Even Inner & Even Outer
6. Odd Outer & Odd Centre & Odd Inner & Even Inner & Even Centre & Even Outer

- `Header [<markdown>...]`: Defines the header of the document. If no header is defined the separating horizontal line is also removed from the page. The header is empty by default.

  Common header document headers typically include the current chapter and/or section of the document. For the purpose of chapter and section numbers the LaTeX functions `\leftmark` and `\rightmark` may be used.

- `Footer [<markdown>...]`: Define the footer of the document. By default the footer put page numbers at the centre of the page on one-sided documents, and at the outer margins for two-sided documents.

  If overridden the page numbers may be manually included using the LaTeX function `\pagemark`. Fore a more fancy pagination, for example of the style _Page X of X_, one can achieve this using: `Page \pagemark of \lastpage`. These LaTeX functions may also be used in the header if desired.

## Title Page

The title page macros define the various parts that make up the title page of the document as well as setting various document metadata.

- `TitlePage [<markdown>...]`: Place title page here! Ideal if you want a notice or some other content before the title page. If the macro is not present in the document the title page will be placed before any other content, but only if the document has a title.

  If any content is provided, use what ever content provided as the title page, ignoring the other title page macros other than for document metadata. Probably most ideal for use with raw LaTeX, for example by importing a separately created title page using the `\includepdf` command.

- `Titlehead <markdown>...`: Defines the header of the title page. The titlehead macro takes a list of arguments in the same manner as described in the header and footer macros.

- `Subject <markdown>`: Defines the subject of the document. Also sets the `subject` field in the document metadata.

- `Title <markdown>`: Defines the title of the document. Also sets the `title` field in the document metadata.

- `Subtitle <markdown>`: Defines the subtitle of the document.

- `Authors <markdown>...`: Defines the author(s) of the document. The `Authors` macro takes a list of inline markdown arguments, one author per line, and may appear multiple times in the document, appending to the list of authors already defined for the document.

- `Author <markdown>...`: Defines the author(s) of the document. The `Author` macro takes a list of inline markdown arguments, though unlike the `Authors` macro, assumes that each line pertains to the same author. This allows for adding additional information about an author, like for example: title, organization or association. For the purpose of document metadata only the first line is used when listing authors.

  **Example**:
  ```
  % Author: Name of Author
            _Professor of Triviality_
            University of Nowheresville
  ```

  The `Author` macro may appear multiple times in the document, each time appending to the list of authors already defined for the document.

- `Date <markdown>`: Defines the date of the document. The LaTeX function `\today` may be practical here.

- `Publisher <markdown>`: Defines the publisher of the document.

- `Keywords <markdown>...`: Defines the keywords of the document. Only does this by setting the `keywords` field in the document metadata. If you want the keywords to be present in the actual rendered text this must be done manually.

## Listings

The listings macros are all inline macros that places the desired list at the macros point of entry in the document. Note that none of the listings macros include a header, if this is desired this must be provided manually.

- `TableOfContents`: Generate a table of contents.

- `ListOf: <box-group>`: Generate a list of boxes for all boxes with the given caption prefix.

- `Notes`: Converts all footnotes to endnotes and prints the resulting list at the macros point of entry in the markdown document. This macro may appear multiple times, each time it will include the notes accumulated up until the point it appears. Useful if you want a list of notes at the end of each chapter for example.

- `Bibliography <filepath>...`: Generates a bibliography using the specified bibliographic database file(s), for example using the BibLaTeX (`.bib`) format. Any format supported by [pandoc](http://pandoc.org/MANUAL.html#citations) is supported here.

  The `Bibliography` macro may appear multiple times in the document, each time it will include the bibliographic entries for all the citations accumulated up until the point it appear.

## Document Flow

- `Part <markdown>...`: Creates a special level 0 header. These headers will fill the whole page marking a major separation in the document, at least in report and book type documents. In articles these types of headers are typically not used. Since these headers are usually numbered differently than the rest of the document, if at all, these headers are never numbered and must be numbered manually if desired.

- `ChapterPrefix [<markdown>]`: Defines the chapter prefix for level 1 headers in the document. This has no effect if the document is of type `Article` as this document form does not define level 1 headers, but rather starts counting from a depth of 2, aka. sections. However, in `Report` or `Book` type documents this adds a separate header line for each numbered chapter in the document.

  This macro may appear multiple times in the document, each time modifying all the chapters following the macro or until the macro appears again in the document.

  Not providing an argument to the `ChapterPrefix` macro clears the chapter prefix.

- `TocDepth [<number>]`: Defines the level to which headers are included in the table of contents, if a table of content is included in the document. Defaults to `0`.

- `NumDepth [<number>]`: Defines the level to which headers are numbered in the document. Defaults to `0` meaning no headers are numbered.

- `NumbStyle <style>`: Defines the numbering style of top-level headers in the document. Style may be either `Numeric` (aliases: `Num` and `Number`), `Alphabetic` (aliases: `Alpha` and `Letter`) or `Roman` (aliases: `Rom`).

- `BmkDepth <number>`: Defines the depth to which headers are included in the bookmarks of the document. Defaults to `3`.

- `BmkReset`: Reset the root level of document bookmarks. Useful when having used the `Part` macro, but you want a chapter afterwords that should not be nested under this part.

### Shorthands

Convenience macros for common document structure.

- `Frontmatter [<chapter-prefix>]`:
  - `NumDepth: 1`
  - `TocDepth: 1`
  - `NumStyle: Roman`
  - `ChapterPrefix[: <chapter-prefix>]`

- `Mainmatter [<chapter-prefix>]`:
  - `NumDepth: 3`
  - `TocDepth: 3`
  - `NumStyle: Numeric`
  - `ChapterPrefix[: <chapter-prefix>]`

- `Appendices [<chapter-prefix>]`:
  - `NumDepth: 3`
  - `TocDepth: 3`
  - `NumStyle: Alphabetical`
  - `ChapterPrefix[: <chapter-prefix>]`

- `Backmatter [<chapter-prefix>]`:
  - `NumDepth`
  - `TocDepth: 3`
  - `NumStyle: Numeric`
  - `BmkReset`
  - `ChapterPrefix[: <chapter-prefix>]`

## Other

- `LinksAsNotes`: Changes all links to footnotes, useful for print.

- `IncludeHead`: Include raw LaTeX into the document header. Useful if you need some extra LaTeX packages, for example `tikz` for rendering custom figures.
