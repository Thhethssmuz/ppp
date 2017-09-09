module Filter.NumberRef (numberRef) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk

replaceInline :: Inline -> Inline -> Inline
replaceInline r (Str "#") = r
replaceInline _ x = x

process :: Inline -> Inline
process (Link a is t@('#':xs, _)) =
  let ref = RawInline (Format "tex") $ "{\\ref{" ++ xs ++ "}}"
  in  Link a (walk (replaceInline ref) is) t
process x = x

numberRef :: Pandoc -> Pandoc
numberRef = walk process
