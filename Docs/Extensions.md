# Extensions

## Boxes

Boxes, traditionally called _floats_ in typographical contexts, is a way of placing and grouping elements that should not be split across pages. Figures and tables are the most common examples of such elements.

Traditionally if such an element is placed on a page where there is not enough space for it to fit it might leave half a page empty before appearing at the top of the next page. This is typically an undesired effect as it breaks the flow of text.

Boxes provide a way of positioning the element such that it does not break the flow of text in such a way. This often means that the element will be placed somewhere other than the actual point of entry in the document, but somewhere near where the flow of text is not disrupted.

In ppp all figures, tables and code blocks are wrapped in boxes. These boxed elements will never be split across pages, however, the default behaviour is still absolute positioning at the point of entry in the markdown document, however, the floating behaviour is easily enabled.

### Figures

![Example figure; first sub caption; second sub caption](ex.jpg ex.jpg)

The syntax for figures have been extended to support simple grouping of multiple figures and figure captions. Based on the normal markdown syntax for images the field for alt-text are used for figure captions. This field may be used to specify multiple captions separated by `;`.

    ![Example figure; first sub caption; second sub caption](...)

The url field of the figure are extended to take a list of urls separated by a space. If the space character is present in the url it may be escaped using the `\` character.

    ![...](ex.jpg ex.jpg)

At the end of the url field there may also be specified an attribute block. This block is used to set properties to control the behaviour of the box encapsulating the figure. The attribute block will be explained in [Section # Attribute Block](#attribute-block).

    ![...](... {ATTRIBUTE BLOCK})

### Tables

Table syntax is the same as for normal pandoc tables, except that the table caption is extended to support an attribute block at the end.

### Code Blocks

Code blocks natively supports attribute blocks in pandoc so this syntax is not extended in any way. However, code blocks do not normally support captions, so to achieve this one may specify a caption attribute in the attribute block.

### Custom

Custom boxes may be defined using an inline html div. In order for ppp to treat the div as a float it must have the class `box`. An additional class determines the type of the box which in turn determines the caption prefix and in which listing the element will appear if any. Current supported types are:

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

Custom boxes were primarily made to be able to group multiple elements together, for example by placing two tables side by side. To accomplish this one may simply wrap the two tables in a box.

    <div id="my-table" class="table box">

    ------------------
    a table...
    ------------------

    ------------------
    an other table...
    ------------------

    <span class="caption">My table</span>
    </div>

<div id="my-table" class="table box">

------------------
a table...
------------------

: {width="1"}

------------------
an other table...
------------------

: {width="1"}

<span class="caption">My table</span>
</div>

Custom boxes may be given a caption by adding a span with the class `caption`, in the case above the caption for the custom box, i.e. the wrapper, will be the main caption for the grouping.

### In-document References

The type of the box also determines the prefix of the in-document reference to the box. All boxes are prefixed with their short names (see [Table #](#tab:box-types) for the full list) followed by `:`. For example the generated reference for the example '[Table #](#tab:my-table)' is `#tab:my-table`.

### Attribute Block

The attribute block is an element wrapped in curly braces (`{}`) containing properties to control the behaviour of the box. The syntax for the content of the attribute block are the same as those for pandoc and will not be explained here, for more info see the [pandoc documentation](http://johnmacfarlane.net/pandoc/README.html#header-identifiers-in-html-latex-and-context).

-------------------------------------------------------------------------------
**Class**  **Description**
---------- --------------------------------------------------------------------
.float     Enables the floating behaviour for the box.

.span      In a multicolumn document lets the box span the whole page

.wrap      Lets text flow around the document (incompatible with .span)

.long      Overwrites most other classes and tries to make the box span
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

## Numbered References

This extension extends the syntax for in-document links and allows auto generated numbers to be included in the link itself. The extension will replace the character `#` with the generated number for the referenced section heading, figure, table, etc..

For example to reference this section you may write:

`See [Section # Numbered References](#numbered-references).`

and ppp will generate:

See [Section # Numbered References](#numbered-references).

