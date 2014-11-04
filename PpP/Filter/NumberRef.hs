module PpP.Filter.NumberRef (numberRef) where

import Text.Pandoc
import Text.Pandoc.Walk

import Data.List.Split (splitOn)
import Data.Maybe      (listToMaybe)
import Data.List       (isInfixOf, intersperse)

isRef :: Target -> Bool
isRef (a, b)
  | take 1 a == "#" = True
  | otherwise       = False

tex :: String -> Inline
tex = RawInline (Format "tex")

ref :: String -> Inline
ref r = tex $ "\\ref{" ++ r ++ "}"

replaceInline :: String -> Inline -> Inline
replaceInline r (Str s) = wrap . intersperse (ref r) . map Str . splitOn "??" $ s
replaceInline r (Emph is)        = Emph $ map (replaceInline r) is
replaceInline r (Strong is)      = Strong $ map (replaceInline r) is
replaceInline r (Strikeout is)   = Strikeout $ map (replaceInline r) is
replaceInline r (Superscript is) = Superscript $ map (replaceInline r) is
replaceInline r (Subscript is)   = Subscript $ map (replaceInline r) is
replaceInline r (SmallCaps is)   = SmallCaps $ map (replaceInline r) is
replaceInline r (Image is t)     = Image (map (replaceInline r) is) t
replaceInline r (Span a is)      = Span a $ map (replaceInline r) is
replaceInline _ inline           = inline

process :: [Inline] -> Target -> Inline
process is t 
  | isRef t   = Link (map (replaceInline . drop 1 . fst $ t) is) t
  | otherwise = Link is t

wrap :: [Inline] -> Inline
wrap is = Span ("", [], []) $ [pre] ++ is ++ [post]
  where pre  = tex "}"
        post = tex "{"

processLinks :: Inline -> Inline
processLinks (Link is to) = process is to
processLinks x = x

numberRef :: Pandoc -> Pandoc
numberRef = walk processLinks
