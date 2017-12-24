# Float

\cleardoublepage

Float

\lipsum[1]
\lipsum[1]
\lipsum[1]

![](test/Test.jpg){.float width=50%}

\lipsum[1]
\lipsum[1]
\lipsum[1]

\cleardoublepage

Float with caption.

\lipsum[1]
\lipsum[1]
\lipsum[1]

![](test/Test.jpg){.float width=50%}

Figure: Caption

\lipsum[1]
\lipsum[1]
\lipsum[1]



\cleardoublepage

% PageColumns: 2

Two-column float is strictly not supported without `.span` so `.float` should implicitly add the `.span` class when inside a multicol environment.

\lipsum[1]
\lipsum[1]
\lipsum[1]

![](test/Test.jpg){.float width=50%}

\lipsum[1]
\lipsum[1]
\lipsum[1]

% PageColumns: 1

\cleardoublepage

% PageColumns: 2

Float + span

\lipsum[1]
\lipsum[1]
\lipsum[1]

![](test/Test.jpg){.float.span width=50%}

\lipsum[1]
\lipsum[1]
\lipsum[1]

% PageColumns: 1

\cleardoublepage

% PageColumns: 2

Although "Here" floats should work fine inside multicol.

\lipsum[1]
\lipsum[1]
\lipsum[1]

![](test/Test.jpg)

\lipsum[1]
\lipsum[1]
\lipsum[1]

% PageColumns: 1
