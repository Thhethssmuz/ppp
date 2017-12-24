module Filter.Multicol (multicol) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk

tex :: String -> Block
tex = RawBlock (Format "tex")

wrap :: Block -> Block
wrap b = Div ("",[],[])
  [ tex $ "%--trim--%\n\\end{pppmulticol}"
  , b
  , tex $ "\\begin{pppmulticol}\n%--trim--%"
  ]

columnize :: Bool -> Block -> Block
columnize False b@(Header 1 _ _) = wrap b
columnize _     b = b

multicol :: Pandoc -> Pandoc
multicol doc@(Pandoc meta _) = walk (columnize b) doc
  where b = case lookupMeta "documentclass" meta of
              Just (MetaString "scrartcl") -> True
              _                            -> False
