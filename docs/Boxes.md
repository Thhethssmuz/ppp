# Boxes

Boxes provide a way of wrapping and positioning content that should not be split across pages. Figures is the obvious example of such elements, though it is often desirable to wrap other types of content like tables, code blocks, quotes, formulas etc.. In ppp standalone images, tables and code blocks are automatically wrapped in boxes.

Another purpose of wrapping these elements is to allow these elements to be positioned independently of the text. This may include wrapping the text around the boxed element (`.wrap`) or floating the element to a place where the flow of text is not disrupted (`.float`). This often means that these elements will be placed somewhere other than the actual point of entry in the document, something which may seem unintuitive, which is why the default behaviour is to maintain the absolute position in the document for these elements... even if it would lead to half a page of blank space!

## Captions

Immediately following a boxed element you may specify a caption for the box. The caption syntax is inspired by [pandoc's table caption extension](http://pandoc.org/MANUAL.html#extension-table_captions), but has been extended slightly to support arbitrary types and attributes.

```
Prefix: Caption content {#id .class attribute="value"}
```

The prefix may be any arbitrary word and will be used as the caption prefix for that element.<sup id="b1">[1](#f1)</sup> This prefix also determines the group the box belongs to such that you may generate a `list of` for them by using the `ListOf` macro.

```
# List of Prefixes
% ListOf: Prefix
```

Captions with no prefix will never be numbered and will not belong to any group. By default standalone images using the standard pandoc markdown caption syntax will use this empty group.

```
![Caption content](/path/to/image.jpg){#id .class attribute="value"}
```

is equivalent to:

```
![](/path/to/image.jpg)

: Caption content {#id .class atribute="value"}
```

<sup id="f1">1</sup>: The caption prefix identifier must be a single word and may not contain spaces or certain special characters, this to minimise the potential ambiguity between captions and plain paragraphs that just happen to contain a `:`. If you want your prefix to consist of multiple words or contain special characters you may escape them using `\`. Inline markdown in the caption prefix is not supported. [↩](#b1)

## Custom Boxes

For other types of elements, that are not automatically wrapped in boxes you may manually wrap them in a `div` with a class `box` using raw `html` or [fenced divs](http://pandoc.org/MANUAL.html#extension-fenced_divs).

```
::: box
The square of the hypotenuse is equal to the sum of the square of the other sides. $$a^2 + b^2 = c^2$$
:::

Theorem: The Pythagorean Theorem {#pyth .float width=50%}
```

## Nested Boxes

Boxes may be nested in order to place them side by side. By default nested elements will be scaled evenly, however, this may be overridden by manually specifying a `width` for each of the sub boxes. The width of a sub box is relative to the parent box.

If the sub float has the same caption prefix as the parent then the caption is treated as a sub-caption of the parent.

```
::: box
------------------
a table...
------------------

Table: Some table {#subtable1 width="50%"}

------------------
an other table...
------------------

Table: Some other table {#subtable2 width="50%"}
:::

Table: Main caption {#maintable .float .span width=100%}
```

## Referencing

In-document references to a box may be done using its `id` as is. Unlike pandoc and previous versions of ppp, figure ids are no longer prefixed with `fig:`. That is not to say that this is not a useful convention, but since the caption prefix may be any arbitrary word ppp does not attempt to generate short names for these labels. So, just keep in mind that you have to apply this convention manually if you want to use it.

```
![](/path/to/image.jpg)

Figure: My figure. {#fig:myfigure}

Here is a paragraph that references [Figure #](#fig:myfigure).
```

## Box Attributes

The attribute block is an element wrapped in curly braces `{}` containing properties to control the behaviour of the box. The syntax for the content of the attribute block are the same as those for various elements in pandoc and will not be explained here, for more info see the [pandoc documentation(http://pandoc.org/MANUAL.html).

| **Class** | **Description** |
|:----------|:----------------|
| `.float` | Enables the floating behaviour for the box. |
| `.span` | In a multicolumn document lets the box span the whole page. |
| `.wrap` | Lets text flow around the box (incompatible with `.span`). |
| `.long` | An unfortunate side effect of automatically wrapping tables and code blocks in boxes is that they absolutely cannot span multiple pages! So to support long tables and code blocks that should span multiple pages, this class undoes the boxing. Only supported for tables and code blocks. |
| `.unnumbered` | Disable numbering for the box. This also removes the box prefix. |
| `.normalise` or `.normalize` | Normalise table widths to fill the entire box width. |
| `.numberLines` | Enable line numbering for code blocks. |

| **Attribute** | **Option** | **Description** |
|:--------------|:-----------|:----------------|
| `width=` || Determines the width of the box in relation to the text width. In a multicolumn document the percentage is in relation to the column unless `.span` is specified then it is in relation to the page. May be specified as a percentage e.g.: "75%" or as a ratio e.g.: "0.75". Defaults to "100%" unless `.wrap` is specified then it defaults to "50%" |
| `align=` || Determines the alignment of the inner content of the box. This attribute takes two arguments separated by a space character, one to specify the vertical alignment and one to specify the horizontal alignment. The order does not matter. Defaluts to `top centre`. |
|| `top` | (Default) Vertical |
|| `centre` or `center` | Vertical |
|| `bottom` | Vertical |
|| `left` | Horizontal |
|| `centre` or `center` | (Default) Horizontal |
|| `justified` | Horizontal |
|| `right` | Horizontal |
| `wrap=` || Determines the alignment of a wrapping box (requires `.wrap`). |
|| `outer` | (Default) Floats the box towards the outer margins of the page. |
|| `left` | Floats the box to the left of the text. |
|| `right` | Floats the box to the right of the text. |
|| `inner` | Floats the box towards the inner margins of the page. |
| `style=` || Set the style of the box. |
|| `plain` | (Default) Renders the box without any borders and with captions placed at the bottom. |
|| `plaintop` | Renders the box without any borders and with captions placed at the top. |
|| `boxed` | Renders the box with border and captions placed on the bottom, underneath the border. |
|| `ruled` | Renders the box with borders on the top and bottom with the caption placed above with an additional border above the caption as well. |
| `table-style=` || Sets the style of a table. |
|| `none` | Renders the table without any rules or borders. |
|| `plain` | (Default) Renders the table with a top, middle and bottom rule. |
| `column-widths=` || Manually specify the column widths for a table. Expects either one argument, for a table with equal column widths, or a number of arguments equal to the number of columns in the table. Arguments are separated by a space and may be specified either as a percentage e.g.: "15%" or as a ratio e.g.: "0.15". If `.normalise` is specified, these widths are treated as relative widths. |
| `startFrom=` || Set the starting line number in a column block (requires `.numberLines`). |
