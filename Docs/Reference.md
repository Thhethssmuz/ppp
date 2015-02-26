% Type    : Article
% Language: British

% Header  : _ppp reference_

% Title   : ppp reference
% Author  : Svein Inge Albrigtsen
% Date    : Arendal, \today

% LinksAsNotes

% TableOfContents

% Mainmatter

# Introduction

ppp is a pandoc preprocessor and is aimed for writing longer academic texts using markdown and adds some small extensions to the markdown syntax aimed to further this goal.

This document will only cover the usage of ppp and its extensions, for information about the pandoc markdown syntax for which it extends, you can see the pandoc documentation available [here](http://johnmacfarlane.net/pandoc/README.html).

## Macros

Most notably ppp adds preprocessing macros for including other files and to set document metadata. Macros begin with the character `%` and must start on a new line. Macros taking arguments must be followed by a `:`. All macros are case insensitive.

There are 2 global macros that are usable by all ppp document types

Include
:   Used to include a separate file into the markdown document. This macro accepts a list of arguments, separated by either a newline or `;`, and includes all files in order at the point of entry in the markdown document.

<div id="include-macro" class="example box" style="ruled" align="bottom left">
<div class="sub box">

    % Include: myfile.md
               my other file.md

</div>
<span class="caption">Include macro</span>
</div>

Type
:   The document type is used to select which document template is used for the document. The `Type` macro may only occur once in a document and determines what macros and other markdown extensions are available in the rest of the document.

:   Currently supported document types are:

:   - Report
    - Article

:   Default is `Report` if no type is specified.


% Include : Report.md


