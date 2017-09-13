module Filter.MultiBib (multibib) where

import Control.Monad.Trans.State
import Data.List.Split (splitWhen)
import Text.Pandoc.Definition
import Text.Pandoc.Walk

splittable :: Inline -> [Inline]
splittable x@(Cite _ _) = [x]
splittable (Span ("",["refmarker"],[]) []) = [Space]
splittable _ = []

cites :: Inline -> [Citation]
cites (Cite cs _) = cs
cites _ = []

refs :: Block -> [Block]
refs d@(Div (_,["references"],_) _) = [d]
refs _ = []

trimPart :: [String] -> Block -> Block
trimPart xs ref@(Div ('r':'e':'f':'-':refid,_,_) bs)
  | refid `elem` xs = ref
  | otherwise = Null
trimPart _ x = x

filterParts :: Block -> State [[String]] Block
filterParts bib@(Div (_,["references"],_) _) = do
  state <- get
  case state of
    (xs:xss) -> do
                put xss
                return $ if length xs > 0 then walk (trimPart xs) bib else Null
    _        -> return Null
filterParts x = return x

filterRefMarkers :: Block -> Block
filterRefMarkers (Plain [Span ("",["refmarker"],[]) []]) = Null
filterRefMarkers x = x

multibib :: Pandoc -> Pandoc
multibib doc =
  let css  = map (map citationId . concatMap cites)
           . splitWhen ((==) Space)
           . query splittable $ doc
      doc' = if length css > 2
               then evalState (walkM filterParts doc) css
               else doc
  in  walk filterRefMarkers doc'
