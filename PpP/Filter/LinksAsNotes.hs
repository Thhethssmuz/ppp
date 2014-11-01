module PpP.Filter.LinksAsNotes (linksAsNotes) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk

markAll :: Inline -> Inline
markAll (Link is t) = Span ("",["link-as-note"],[]) [Link is t]
markAll x = x

unmark :: Inline -> Inline
unmark (Span ("",["link-as-note"],[]) [l]) = l
unmark x = x

unmarkNotes :: Inline -> Inline
unmarkNotes (Note bs) = Note . walk unmark $ bs
unmarkNotes x = x

markedToNote :: Inline -> Inline
markedToNote (Span ("",["link-as-note"],[]) [(Link is (url, title))]) = 
  Span ("",[],[]) $
  is ++ [Note [Para [Link [Str url] (url, title)]]]
markedToNote x = x

linksAsNotes :: Pandoc -> Pandoc
linksAsNotes doc = walk markedToNote
                 . walk unmarkNotes
                 . walk markAll $ doc
