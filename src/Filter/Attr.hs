module Filter.Attr (simplifyAttr) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared (splitBy)
import Text.Pandoc.Walk

simplify :: Attr -> Attr
simplify (i, cs, as) =
  let (i':cs1) = if null i then [i] else splitBy (== '.') i
      cs2      = concatMap (splitBy (== '.')) cs
  in  (i', cs1 ++ cs2, as)

inline :: Inline -> Inline
inline (Code a x) = Code (simplify a) x
inline (Link a x y) = Link (simplify a) x y
inline (Image a x y) = Image (simplify a) x y
inline (Span a x) = Span (simplify a) x
inline x = x

block :: Block -> Block
block (CodeBlock a x) = CodeBlock (simplify a) x
block (Header x a y) = Header x (simplify a) y
block (Div a x) = Div (simplify a) x
block x = x

simplifyAttr :: Pandoc -> Pandoc
simplifyAttr doc = walk block . walk inline $ doc
