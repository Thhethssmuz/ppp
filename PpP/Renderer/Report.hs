module PpP.Renderer.Report (renderReport, renderArticle) where

import PpP.Err
import PpP.Filter
import PpP.Shared
import PpP.Language
import PpP.Renderer.Shared

import Text.Pandoc
import Text.Pandoc.PDF
import Text.Pandoc.Walk
import Text.Highlighting.Kate.Styles
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map.Lazy as M

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char (toLower)
import Data.Maybe (isJust)

import Paths_ppp
import System.FilePath
import System.Exit



config :: Unprocessed -> State PpP ()
config (Markdown s)  = add "" s
config (Macro k v)   = case k of

  "language"        -> let lang = map toLower v
                           loc  = M.lookup lang languages in
                       case loc of
                         Nothing -> do
                                    add "err" . pppErr  $ "unsupported language " ++ lang
                                    addOnce k ""
                         Just l  -> addOnce k $ metaVar "lang" lang ++
                                                metaVar "locale" l

  "header"          -> let hs = parseList v in
                       case length hs of
                         0 -> return ()
                         1 -> addOnce k $ metaVar "page-header-centre" (head hs)
                         2 -> addOnce k $ metaVar "page-header-left"   (head hs) ++
                                          metaVar "page-header-right"  (last hs)
                         3 -> addOnce k $ metaVar "page-header-left"   (head hs) ++
                                          metaVar "page-header-centre" (hs !! 1) ++
                                          metaVar "page-header-right"  (last hs)
                         _ -> do 
                              add "err" . pppErr $ "to many arguments applied to macro " ++ k
                              config . Macro k . unlines . take 3 $ hs

  "footer"          -> let fs = parseList v in
                       case length fs of
                         0 -> return ()
                         1 -> addOnce k $ metaVar "page-footer-centre" (head fs)
                         2 -> addOnce k $ metaVar "page-footer-left"   (head fs) ++
                                          metaVar "page-footer-right"  (last fs)
                         3 -> addOnce k $ metaVar "page-footer-left"   (head fs) ++
                                          metaVar "page-footer-centre" (fs !! 1) ++
                                          metaVar "page-footer-right"  (last fs)
                         _ -> do 
                              add "err" . pppErr $ "to many arguments applied to macro " ++ k
                              config . Macro k . unlines . take 3 $ fs

  "page"            -> forM_ (map (map toLower) . parseList $ v) $ \x -> do
                         case x of
                           "twoside"   -> addOnce x $ metaVar "page-twoside" "true"
                           "twocolumn" -> addOnce x $ metaVar "page-twocolumn" "true"
                           _           -> add "err" . pppErr $ "unknown argument '" ++ x ++ "' applied to page macro"

  "titlehead"       -> addOnce k $ metaBlock k v
  "subject"         -> addOnce k $ metaBlock k v
  "title"           -> addOnce k $ metaBlock k v
  "subtitle"        -> addOnce k $ metaBlock k v
  "author"          -> addOnce k $ metaList k v
  "date"            -> addOnce k $ metaBlock k v
  "publisher"       -> addOnce k $ metaBlock k v
  "keywords"        -> addOnce k $ metaList k v
  "abstract"        -> addOnce k $ metaBlock k v

  "numbersections"  -> add k $ inlineFunc' k v
  "tocdepth"        -> add k $ inlineFunc' "tocdepth" v

  "toc"             -> addOnce k $ inlineFunc k ""
  "lof"             -> addOnce k $ inlineFunc k ""
  "lot"             -> addOnce k $ inlineFunc k ""

  "appendices"      -> addOnce' k $ inlineFunc k ""

  "csl"             -> addOnce k $ metaVar k v

  "notes"           -> do
                       addOnce k $ metaVar "notes-heading" "true"
                       config . Macro "notes\'" $ v
  "notes\'"         -> let v' = map toLower v in
                       addOnce k $ inlineFunc "notes" "" ++
                       case v' of
                         "simple"  -> metaVar "notes-chapter" "true"
                         "grouped" -> metaVar "notes-chapter" "true" ++
                                      metaVar "grouped-notes" "true"
                         ""        -> metaVar "notes" "true"
                         _         -> metaVar "notes" "true" ++
                                      pppErr ("unknown argument " ++ v' ++
                                              " applied to macro notes")
  "citations"       -> do
                       addOnce k $ metaVar "cites-heading" "true"
                       config . Macro "citations\'" $ v
  "citations\'"     -> addOnce k $ metaVar "cites" "true" ++
                                   inlineFunc "citations" ""
  "bibliography"    -> do
                       addOnce k $ metaVar "bib-heading" "true"
                       config . Macro "bibliography\'" $ v
  "bibliography\'"  -> addOnce k $ metaVar "bibliography" v ++
                                   inlineFunc "bibliography" ""

  "pagesize"        -> addOnce k $ metaVar "page-size" v
  "pagediv"         -> addOnce k $ metaVar "page-div" v

  "fontsize"        -> addOnce k $ metaVar "font-size" v
  "mainfont"        -> addOnce k $ metaVar "main-font" v
  "sansfont"        -> addOnce k $ metaVar "sans-font" v
  "monofont"        -> addOnce k $ metaVar "mono-font" v
  "mathfont"        -> addOnce k $ metaVar "math-font" v

  _                 -> add "err" . pppErr $ "unknown macro " ++ k
config (Include k _) = add "err" . pppErr $ "unknown macro " ++ k


renderArticle :: [Unprocessed] -> FilePath -> IO ()
renderArticle doc out = do
  template  <- readFile =<< getDataFileName ("tex" </> "report.tex")
  let ppp    = execState (mapM_ config doc) academicPpP{
                 writer = academicWriter{
                   writerVariables = [
                     ("documentclass", "scrartcl"),
                     ("rootlevel", "1"),
                     ("article", "true")
                   ],
                   writerChapters = False,
                   writerTemplate = template
                 }
               }
  pandoc    <- pppToPandoc ppp ""
  renderPDF pandoc (writer ppp) out

renderReport :: [Unprocessed] -> FilePath -> IO ()
renderReport doc out = do
  template  <- readFile =<< getDataFileName ("tex" </> "report.tex")
  let ppp    = execState (mapM_ config doc) academicPpP{
                 writer = academicWriter{
                   writerVariables = [
                     ("documentclass", "scrreprt"),
                     ("rootlevel", "0"),
                     ("report", "true")
                   ],
                   writerChapters = True,
                   writerTemplate = template
                 }
               }
  pandoc    <- pppToPandoc ppp ""
  renderPDF pandoc (writer ppp) out



pppToPandoc :: PpP -> String -> IO Pandoc
pppToPandoc ppp prefix = do
  pandoc <- fmap toTex
          . wikiref'
          . numberRef
          . figure
          . linksAsNotes
          . readMarkdown (reader ppp)
          $ document ppp
  printErrors pandoc
  return pandoc

renderPDF :: Pandoc -> WriterOptions -> String -> IO ()
renderPDF pandoc writer' out = do
  putStrLn $ "rendering " ++ out
  pdf <- makePDF "xelatex" writeLaTeX writer' pandoc
  case pdf of
    Left err -> do
                BS.putStrLn err
                exitFailure
    Right bs -> BS.writeFile out bs

academicWriter :: WriterOptions
academicWriter = def{
  writerStandalone = True,
  writerHighlight = True,
  writerHighlightStyle = tango
}

academicPpP :: PpP
academicPpP = emptyPpP{
  reader = def{
    readerSmart = True,
    readerStandalone = True,
    readerParseRaw = True
  }
}
