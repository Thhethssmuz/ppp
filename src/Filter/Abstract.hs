module Filter.Abstract (abstract) where

import Data.Maybe (fromMaybe)
import Data.List (groupBy)
import Text.Pandoc.Builder ((<>), toList, rawInline, plain, fromList)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)

tex = rawInline "tex"

mkAbstract :: Int -> [Inline] -> [Block] -> [Block]
mkAbstract l is bs = toList
  $ plain ( tex "\\end{pppmulticol}"
         <> tex "\\begin{abstract}["
         <> fromList is
         <> tex "]%\n"
         <> tex "%--trim--%"
          )
 <> fromList bs
 <> plain ( tex "%--trim--%\n"
         <> tex "\\end{abstract}\n"
         <> tex "\\begin{pppmulticol}"
          )

classOffset "scrartcl" = -1
classOffset _          = 0

processSpecialHeaders :: String -> [Block] -> [Block]
processSpecialHeaders c (b@(Header l (_,cs,_) is) : bs)
  | elem "abstract" cs = mkAbstract (l + classOffset c) is bs
  | otherwise          = b:bs
processSpecialHeaders _ bs = bs

wrapSpecialHeaders :: String -> [Block] -> [Block]
wrapSpecialHeaders c bs = concatMap (processSpecialHeaders c) $ groupBy headers bs
  where
    isHeader (Header _ _ _) = True
    isHeader _              = False
    headers x y = isHeader x && not (isHeader y)

abstract :: Pandoc -> Pandoc
abstract doc@(Pandoc meta bs) = walk (wrapSpecialHeaders c) doc
  where
    c = fromMaybe "scrartcl" $ unstr =<< lookupMeta "documentclass" meta
    unstr (MetaString x) = Just x
    unstr _ = Nothing
