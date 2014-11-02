module PpP.Filter.ToTex (toTex) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk

pppToLaTeX :: Block -> Block
pppToLaTeX (Div (_, ["ppp-fn"], [("name", name), ("arg", arg)]) _) = 
  Div ("",[],[]) $ [
    RawBlock (Format "tex") $ "\\" ++ name ++ "{" ++ arg ++ "}"]

pppToLaTeX (Div (_, ["ppp-fn"], [("name", name)]) inner) =
  Div ("",[],[]) $ [
    RawBlock (Format "tex") $ "\\" ++ name ++ "{"] ++ inner ++ [
    RawBlock (Format "tex") "}"]

pppToLaTeX x = x

toTex :: Pandoc -> Pandoc
toTex = walk pppToLaTeX
