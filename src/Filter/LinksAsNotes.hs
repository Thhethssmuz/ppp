module Filter.LinksAsNotes (linksAsNotes) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk

f :: Inline -> Inline
f x@(Link _ _ (('#':_), _)) = x -- ignore in document links
f (Link attr is (url, title)) =
  Span ("",[],[]) $ is ++ [Space, Note [Para [Link attr [Str url] (url, title)]]]
f x = x

linksAsNotes :: Pandoc -> Pandoc
linksAsNotes = walk f
