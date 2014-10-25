module PpP.Filter.ToTex (toTex) where

import Text.Pandoc
import Text.Pandoc.Walk

pppToLaTeX :: Block -> Block
pppToLaTeX (Div ("PpP-Fn", [c], _) i) = Div ("",[],[]) $ [
  RawBlock (Format "tex") $ "\\" ++ c ++ "{"] ++ i ++ [
  RawBlock (Format "tex") "}"]
pppToLaTeX x = x

toTex :: Pandoc -> Pandoc
toTex = walk pppToLaTeX
