module PpP.Filter.Reference (reference', reference) where

import PpP.Shared

import Data.Ord (comparing)
import Data.Char (isAlphaNum, isSpace)
import Data.List (intersperse, findIndices, sortBy, nub)
import Data.Maybe (fromMaybe, listToMaybe, isJust, fromJust)

import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Shared (stringify)
import Text.CSL.Pandoc
import qualified Text.CSL as CSL
import qualified Text.CSL.Style as Style

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Lazy as M

import Paths_ppp
import System.FilePath
import System.Directory (doesFileExist)



insertDiv :: String -> Block -> Block -> Block
insertDiv s bs x@(Div (did, cs, [("name", s')]) _) = 
  if s == s' then Div (did, cs, [("name", s)]) [bs]
             else x
insertDiv _ _ x = x

citations :: Inline -> [Inline]
citations c@(Cite _ _) = [c]
citations _ = []

referenceDiv :: Block -> [Block]
referenceDiv (Div (_,["references"],[]) bs) = bs
referenceDiv _ = []

removeReferencesDiv :: Block -> Block
removeReferencesDiv (Div (_, ["references"], []) _) = Null
removeReferencesDiv x = x



marker :: String
marker = "#-!-YOU-SHOULD-NOT-BE-ABLE-TO-SEE-THIS-!-#"

insertMarker :: CSL.Style -> CSL.Style
insertMarker style = 
  let bib  = fromJust . Style.biblio $ style
      lay  = Style.bibLayout bib
      elm  = Style.elements lay
      m    = Style.Const marker Style.emptyFormatting
      elm' = (\(x:xs) -> x:m:xs) elm
      lay' = lay   { Style.elements  = elm' }
      bib' = bib   { Style.bibLayout = lay' }
  in         style { Style.biblio    = Just bib' }

replaceMarker :: Inline -> Inline
replaceMarker (Str m) | m == marker = tex "]"
                      | otherwise   = Str m
replaceMarker x = x

tex :: String -> Inline
tex = RawInline (Format "tex")

paraToTable :: Block -> Block
paraToTable (Para is) = 
  Para $ [
    tex "\\begin{itemize}\n",
    tex "\\item[" ] ++ is ++ [
    tex "\n\\end{itemize}"
    ]
paraToTable x = x

mkTable :: Block -> Block
mkTable (Div (_,_,_) ps) = Div ([],[],[]) . map paraToTable $ ps
mkTable x = x



reference :: CSL.Style -> [CSL.Reference] -> Pandoc -> Pandoc
reference style bib pandoc@(Pandoc meta _) =
  let flush  = lookupMeta "bib-flush-hack" $ meta

      style' = if   isJust flush
               then insertMarker style
               else style

      doc    = processCites style' bib pandoc
      ref    = Div ([],[],[])
             . query referenceDiv $ doc
      doc'   = walk removeReferencesDiv doc

      ref'   = if   isJust flush
               then mkTable . walk replaceMarker $ ref
               else ref

  in         walk (insertDiv "bibliography" ref') doc'

reference' :: Pandoc -> IO Pandoc
reference' pandoc@(Pandoc meta _) = do
  defcsl  <- getDataFileName $ "csl" </> "ieee.csl"

  csl     <- CSL.readCSLFile ( fmap stringify'
                             . lookupMeta "locale" $ meta )
           . fromMaybe defcsl
           . fmap stringify'
           . lookupMeta "csl" $ meta

  bib     <- fmap concat 
           . mapM CSL.readBiblioFile
           . maybe [] (parseList . stringify')
           . lookupMeta "bibliography" $ meta

  return   . reference csl bib $ pandoc


stringify' :: MetaValue -> String
stringify' (MetaString s) = s
stringify' (MetaInlines is) = stringify is
stringify' (MetaList is) = concat . intersperse ";" . map stringify $ is
