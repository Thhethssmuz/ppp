module PpP.Filter.Multicol (multicol) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Shared (stringify)

parseCols :: String -> Int
parseCols = f . reads
  where f [(x, _)] = if x > 0 then x else 1
        f _ = 1

tex = RawBlock (Format "tex")

colomnize :: Int -> Block -> Block
colomnize 1 b = b
colomnize n h@(Header 1 _ _) = Div ([],[],[]) $ [
  tex "\\end{multicols}", h, tex $ "\\begin{multicols}{" ++ show n ++ "}"]
colomnize n b = b

multicol :: Pandoc -> Pandoc
multicol pandoc@(Pandoc meta _) = 
  let c = maybe 1 (parseCols . stringify')
        . lookupMeta "page-columns" $ meta
  in      walk (colomnize c) pandoc

stringify' :: MetaValue -> String
stringify' (MetaString s) = s
stringify' (MetaInlines is) = stringify is
stringify' _ = "1"
