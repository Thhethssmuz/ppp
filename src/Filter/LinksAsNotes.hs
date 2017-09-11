module Filter.LinksAsNotes (linksAsNotes) where

import Data.Maybe (isJust)
import Text.Pandoc.Definition
import Text.Pandoc.Walk

f :: Inline -> Inline
f x@(Link _ _ (('#':_), _)) = x -- ignore in document links
f (Link attr is (url, title)) =
  Span ("",[],[]) $ is ++ [Space, Note [Para [Link attr [Str url] (url, title)]]]
f x = x

linksAsNotes' :: Pandoc -> Pandoc
linksAsNotes' = walk f

linksAsNotes :: Pandoc -> Pandoc
linksAsNotes doc@(Pandoc meta _)
  | isJust $ lookupMeta "links-as-notes" meta = linksAsNotes' doc
  | otherwise = doc
