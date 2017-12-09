module Filter.Hyperref (hyperref) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)

tex :: String -> Inline
tex = RawInline (Format "tex")

f :: Inline -> Inline
f x@(Link attr is (('#':id), title)) = Span nullAttr $
  [ tex $ "\\hyperref[" ++ id ++ "]{" ] ++ is ++ [ tex "}" ]
f x = x

hyperref :: Pandoc -> Pandoc
hyperref = walk f
