module PpP.Filter.Multicol (multicol, getPageColumns) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Shared (stringify)

parseCols :: String -> Int
parseCols = f . reads
  where f [(x, _)] = if x > 0 then x else 1
        f _ = 1

tex       = RawBlock (Format "tex")

wrap :: Block -> [Block]
wrap b = [ tex $ "\\end{pppmulticol}",
           b,
           tex $ "\\begin{pppmulticol}" ]

columnize :: Int -> Block -> Block
columnize 1 b = b
columnize n h@(Header 1 _ _) = Div ([],[],[]) $ wrap h
columnize n b = b

getPageColumns :: Pandoc -> Int
getPageColumns (Pandoc meta _) =
  maybe 1 (parseCols . stringify') . lookupMeta "page-columns" $ meta

multicol :: Pandoc -> Pandoc
multicol pandoc = walk (columnize . getPageColumns $ pandoc) pandoc

stringify' :: MetaValue -> String
stringify' (MetaString s) = s
stringify' (MetaInlines is) = stringify is
stringify' _ = "1"
