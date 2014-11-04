module PpP.Filter.LinksAsNotes (linksAsNotes) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk

mark :: Inline -> Inline
mark l@(Link _ (('#':_), _)) = l
mark l@(Link _ (('f':'i':'g':':':_), _)) = l
mark l@(Link _ _) = Span ("",["link-as-note"],[]) [l]
mark x = x

unmark :: Inline -> Inline
unmark (Span ("",["link-as-note"],[]) [l]) = l
unmark x = x

unmarkNotes :: Inline -> Inline
unmarkNotes (Note bs) = Note . walk unmark $ bs
unmarkNotes x = x

markedToNote :: Inline -> Inline
markedToNote (Span ("",["link-as-note"],[]) [(Link is (url, title))]) = 
  Span ("",[],[]) $
  is ++ [Space, Note [Para [Link [Str url] (url, title)]]]
markedToNote x = x

linksAsNotes :: Pandoc -> Pandoc
linksAsNotes doc = walk markedToNote
                 . walk unmarkNotes
                 . walk mark $ doc
