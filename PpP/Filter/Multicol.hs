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

columnize :: Bool -> Int -> Block -> Block
columnize _    1 b                = b
columnize True n h@(Header 1 _ _) = Div ([],[],[]) $ wrap h
columnize _    n b                = b

getPageColumns :: Pandoc -> Int
getPageColumns (Pandoc meta _) =
  maybe 1 (parseCols . stringify') . lookupMeta "page-columns" $ meta

multicol :: Bool -> Pandoc -> Pandoc
multicol m pandoc = walk (columnize m . getPageColumns $ pandoc) pandoc

stringify' :: MetaValue -> String
stringify' (MetaString s) = s
stringify' (MetaInlines is) = stringify is
stringify' _ = "1"
