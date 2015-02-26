# Report

## Macros

### Language

Language
:   Sets the language of the document, controls the names of the generated chapter headings, caption markers and terms in the bibliography.

:   Currently supported languages:

:   - English
    - British
    - American
    - Norsk
    - Nynorsk

:   Default is `English`.

### Page Layout

Page
:   ...

    - OneSide
    - TwoSide

    Default is `OneSide`.

PageCols
:   Sets the number of columns of the document, default is 1.

PageSize
:   Sets the size of the document page, values may be any of the following

    - A0, A1, A2 ... A12
    - B0, B1, B2 ... B12

    default is `A4`.

PageDiv
:   default is `10`.

PageBcor
:   default is `0`.

### Fonts

FontSize
:   Sets the font size of the document. Default is `12pt`.

FontMain
:   Sets the main font of the document, the font must be available on the system. Default is `Open Sans`.

FontSans
:   Sets the sans font of the document, the font must be available on the system. Default is `Open Sans`.

FontMono
:   Sets the monospace font for the document. Used by verbatim text and code blocs. The font must be available on the system. Default is `Ubuntu Mono`.

FontMath
:   Sets the math font for the document. Used by formulas and equations encapsulated in `$` or `$$`. The font must be available on the system. Default is selected by \LaTeX and usually is either `Computer Modern` or `Latin Modern Math`.

### Header and Footer

Header
:   The header for document pages. Takes a list of 1, 2 or 3 arguments. Depending on the number of arguments the header are positioned as follows:

:   1. Centre
    2. Left & Right
    3. Left & Centre & Right

:   If a setup other than the above is required, for example a single left entry, an empty dud argument of '-' can be inserted to fill the empty slot.

:   On two sided documents the order is reversed for even pages, such that left equates to the outer margin of the page.

:   If no header is defined the separating horizontal line is also removed from the page. The header is empty by default.

Footer
:   Sets the footer of the document. The footer macro takes a list of arguments in the same manner as described in the header macro.

:   Default the footer put page numbers at the centre of the page on one sided documents, and at the outer margins for two sided documents.

:   If overridden the page numbers may be manually included using the \LaTeX function `\pagemark`. Fore more fancy pagination, for example of the style _Page X of X_, one can use the \LaTeX function `\lastpage`.

### Title Page

Titlehead
:   Sets the header of the title page. The title head macro takes a list of arguments in the same manner as described in header macro.

Subject
:   Sets the subject of the document.

Title
:   Sets the title of the document.

Subtitle
:   Sets the subtitle of the document.

Author
:   Sets the author(s) of the document. The author macro takes a list arguments.

Date
:   Sets the date of the document. The \LaTeX macro `\today` may be of use here.

Publisher
:   Sets the publisher of the document.

Keywords
:   Sets the keywords of the document. The keywords macro takes a list of arguments.

Abstract
:   Sets the abstract of the document.

Dedication
:   TBI

### Listings

The listing macros are all inline macros that places the decried list at the point of entry in the document. Only one of each may be present in a document.

TableOfContents
:   ...

ListOfFigures
:   ...

ListOfTables
:   ...

ListOfPrograms
:   ...

ListOfExamples
:   ...


### Sectioning

Documents are divided into 4 main parts:

Frontmatter
:   The front matter does not number section headings nor does it include them in the table of contents. Every document starts out in this mode so there is no macro called `Frontmatter`.

Mainmatter
:   This part numbers section headings and includes headings in the table of contents. The `Mainmatter` macro may only occur once in a document, and once activated it cannot be deactivated by any other means than to transition to either the appendix or back matter.

Appendix
:   This part changes section numbering to alphabetic and prefixes level 1 headings with the language dependant variant of _Appendices_. Under this style all level 1 headings are treated as separate appendices. The `Appendix` macro may only occur once in a document, and once activated it cannot be deactivated by any other means than a transition to the back matter.

:   The `Appendix` macro may also be used to include external files similar to the `Include` macro. If doing so the `Appendix` part of the document is activated before the first included document.

Backmatter
:   This style does not number section headings, however, it does include sections in the table of content. The `Backmatter` macro may only occur once in a document and once activated it lasts through the rest of the document.

### Numbering

TocDepth
:   Sets the depth to which headings are included in the table of contents. The `tocdepth` macro is an inline function and may be used to selectively change parts of the document. Default behaviour is to not include anything before the toc and everything after the toc down to a default header level of 3. If the `tocdepth` macro appear before the `tableofcontents` macro then the toc will not modify the already set depth otherwise the `tableofcontent` macro will set the depth to 3.

NumDepth
:   Sets the depth to which headings are numbered in the document. The `numdepth` macro is an inline function and may be used to selectively change parts of the document. Only valid in the main matter or appendix part of the document. If the macro occurs before the main matter the depth is applied once the main matter is enabled. If it is applied in the back matter the macro is ignored.

BmkDepth
:   TBI

### Notes

LinksAsNotes
:   Includes all links as footnotes, useful for print.

Notes
:   TBI

### Bibliography

Bibliography
:   The `Bibliography` macro uses pandoc-citeproc to generate a list of bibliographic entries from a database like file. The macro takes a list of arguments, each a bibliographic database file, and prints the bibliography at the point of entry in the markdown file. No header is generated for this list so you may find it useful to precede the macro with a sensible heading.

CSL
:   The `CSL` macro is used to select what style the bibliography is printed in. The macro takes one argument and it must be a valid .csl file. Default stile is IEEE.

## Extensions

### Boxes

Boxes, typically known as floats in typographical contexts, is a way of placing and grouping elements that should not be split across pages. Figures and tables are the most common examples of such elements.

Traditionally if such an element is placed on a page where there is not enough space for it to fit it might leave half a page empty before appearing at the top of the next page. This is typically an undesired effect as it breaks the flow of text.

Boxes provide a way of positioning the element such that it does not break the flow of text in such a way. This often means that the element will be placed somewhere other than the actual point of entry in the document, but somewhere near where the flow of text is not disrupted.

In ppp all figures, tables and code blocks are wrapped in such boxes by default. These boxes will never be split across pages, however, the default behaviour is still absolute positioning at the point of entry in the markdown document, however, the floating behaviour of these boxes are easily enabled.

#### Figures

The syntax for figures have been extended to support simple grouping of multiple figures and figure captions. Based on the normal markdown syntax for images the field for alt-text are used for figure captions. This field may be used to specify multiple captions separated by `;`.

    ![Main caption; first sub caption; second sub caption](...)

The url field of the figure are extended to take a list of urls separated by a space. If the space character is present in the url it may be escaped using the `\` character.

    ![...](img1.png img2.jpg my\ image.png)

At the end of the url field there may also be specified an attribute block. This block is used to set properties to control the behaviour of the box encapsulating the figure. The attribute block will be explained in [Section # Attribute Block](#attribute-block).

#### Tables

Table syntax is the same as for normal pandoc tables, except that the table caption is extended to support an attribute block at the end.

#### Code Blocks

Code blocks natively supports attribute blocks in pandoc so this syntax is not extended in any way.

#### Custom

Custom boxes may be defined using inline html div. In order for ppp to treat the div as a box it must have the class `box`. An additional class determines the type of the box which in turn determines the caption prefix and in which listing the box will appear if any. Current types are:

----------------------
**Type**   **Short**
---------- -----------
figure     fig

table      tab

formula    from

program    prog

example    ex
----------------------

: Box types {#box-types}

<div id="custom-box" class="example box" width="100%" style="ruled" align="bottom centre">
<div class="sub box">

    <div id="myexample" class="example box">
    <div class="sub box">
    **A**
    </div>
    <div class="sub box">
    **B**
    </div>
    <span class="caption">My example</span>
    </div>

<span class="caption">Markdown</span>
</div>
<div class="sub box">

<div class="example box" style="plain">
<div class="sub box">
**A**
</div>
<div class="sub box">
**B**
</div>
<span class="caption">My example</span>
</div>

<span class="caption">Rendered</span>
</div>
<span class="caption">Custom box</span>
</div>

#### In-document References

The type of the box also determines the prefix of the in-document reference to the box. All boxes are prefixed with their short names, see [Table #](#tab:box-types) for the full list. For example to reference the example given in [Examlpe #](#ex:custom-box) the generated reference is `#ex:myexample`.

#### Attribute Block

The attribute block is an element wrapped in curly braces (`{}`) containing properties to control the behaviour of the box. The syntax for the content of the attribute block are the same as those for pandoc and will not be explained here, for more info see the [pandoc documentation](http://johnmacfarlane.net/pandoc/README.html#header-identifiers-in-html-latex-and-context).

-------------------------------------------------------------------------------
**Class**  **Description**
---------- --------------------------------------------------------------------
.float     Lets latex position the box at a place where latex deems it fitting.

.span      In a multicolumn document lets the box span the whole page

.wrap      Lets text flow around the document (incompatible with .span)

.long      Overwrites all other classes and tries to make the box span
           multiple pages if necessary. Currently only works for tables and
           code blocks.
-------------------------------------------------------------------------------

: Box classes {#box-classes}


-------------------------------------------------------------------------------
**Attribute** **Option** **Description**
------------- ---------- ------------------------------------------------------
wrap                     determines the alignment of a wrapping box (requires
                         .wrap)

              left       floats the box to the left of the text

              right      floats the box to the right of the text

              outer      (default) floats the box towards the outer margins of
                         the page 

              inner      floats the box towards the inner margins of the page

width                    Determines the width of the box in relation to the
                         page. In a multicolumn document the percentage is in
                         relation to the column unless .span is specified then
                         it is in relation to the page. May be specified as a
                         percentage e.g.: "50%" or as a ratio e.g.: "0.5".

style                    Set the style of the box

              plain      (Default) Renders the box without any borders and with
                         captions placed at the bottom.

              plaintop   Renders the box without any borders and with captions
                         placed at the top.

              boxed      Renders the box with border and captions placed on the
                         bottom, underneath the border.

              ruled      Renders the box with borders on the top and bottom
                         with the caption placed above with an additional
                         border above the caption as well.

tablestyle               Sets the style of a table, only available for tables
                         not using .long.

              plain      Renders the table without any rules or borders

              ruled      (Default) Renders the table with a top, middle and
                         bottom rule.

align                    Determines the alignment of the inner content of the
                         box. This attribute takes two arguments separated by
                         a space character. First argument to specify the
                         vertical alignment and the second to specify the
                         horizontal alignment.

              top        Vertical

              centre /   Vertical
              center

              bottom     Vertical

              left       Horizontal

              centre /   Horizontal
              center

              justified  Horizontal

              right      Horizontal
-------------------------------------------------------------------------------

: Box attributes {#box-attributes.long}

### Numbered References

This extension extends the syntax for in-document links and allows auto generated numbers to be included in the link itself. The extension will replace the character `#` with the generated number for the referenced section heading, figure, table, etc..


<div id="numbered-references" class="example box" style="ruled" align="bottom left">
<div class="sub box">

`See [Section # Numbered References](#numbered-references).`

<span class="caption">Markdown</span>
</div>
<div class="sub box" align="centre centre">
  
See [Section # Numbered References](#numbered-references).

<span class="caption">Rendered</span>
</div>
<span class="caption">Numbered reference</span>
</div>
