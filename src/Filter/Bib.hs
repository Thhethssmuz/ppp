module Filter.Bib (bibliography) where

import Paths_ppp
import Emb (emb)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import qualified Text.CSL as CSL
import Text.CSL.Pandoc (processCites)
import qualified Text.CSL.Style as Style
import Text.Pandoc.Definition
import Text.Pandoc.Walk

tex :: String -> Block
tex = RawBlock (Format "tex")

tex' :: String -> Inline
tex' = RawInline (Format "tex")


lookupMeta' :: String -> Pandoc -> Maybe MetaValue
lookupMeta' x (Pandoc meta _) = lookupMeta x meta

fromMetaString :: Maybe MetaValue -> Maybe String
fromMetaString (Just (MetaString x)) = Just x
fromMetaString _ = Nothing

fromMetaList :: Maybe MetaValue -> [String]
fromMetaList (Just (MetaList xs)) = catMaybes . map (fromMetaString . Just) $ xs
fromMetaList _ = []


secondFieldAlignFlush :: CSL.Style -> Bool
secondFieldAlignFlush style = fromMaybe False $ do
  bib <- CSL.biblio style
  sfa <- lookup "second-field-align" . CSL.bibOptions $ bib
  return $ sfa == "flush"

hackFlush :: CSL.Style -> CSL.Style
hackFlush style =
  let newbib = do
               bib    <- CSL.biblio style
               let lay = CSL.bibLayout bib
                   m x = Style.Const x Style.emptyFormatting
                   xs  = Style.elements lay
                   xs' = [m "<pppref>"] ++ take 1 xs ++ [m "</pppref>"] ++ drop 1 xs
               return  $ bib { CSL.bibLayout = lay { Style.elements = xs' } }
  in  style { CSL.biblio = newbib }

replaceBibMarkers :: Inline -> Inline
replaceBibMarkers (RawInline (Format "html") "<pppref>" ) = tex' "\\item["
replaceBibMarkers (RawInline (Format "html") "</pppref>") = tex' "]"
replaceBibMarkers x = x

wrapBibliography :: Block -> Block
wrapBibliography (Div ("refs",_,_) bs) =
  Div ("",[],[]) $ [tex "\\begin{itemize}"] ++ bs ++ [tex "\\end{itemize}"]
wrapBibliography x = x


bibliography' :: Pandoc -> IO Pandoc
bibliography' doc = do
  style     <- case fromMetaString $ lookupMeta' "csl" doc of
                    Nothing -> return
                             . CSL.parseCSL'
                             . LBS.fromStrict
                             . fromJust
                             . lookup "ieee.csl" $ emb
                    Just fp -> fmap CSL.parseCSL' . LBS.readFile $ fp

  let flush  = secondFieldAlignFlush style
      style' = if flush then hackFlush style else style

  style''   <- CSL.localizeCSL (fromMetaString $ lookupMeta' "csllocale" doc) style'
  bib       <- fmap concat
             . mapM CSL.readBiblioFile
             . fromMetaList
             . lookupMeta' "bibliography" $ doc

  let doc'   = processCites style'' bib doc
      doc''  = if flush
                 then walk wrapBibliography . walk replaceBibMarkers $ doc'
                 else doc'

  return doc''

bibliography :: Pandoc -> IO Pandoc
bibliography doc
  | isJust $ lookupMeta' "bibliography" doc = bibliography' doc
  | otherwise = return doc
