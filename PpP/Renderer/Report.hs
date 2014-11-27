module PpP.Renderer.Report (renderReport, renderArticle, renderJournal) where

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

  "titlehead"       -> addOnce k $ metaBlock k v
  "subject"         -> addOnce k $ metaBlock k v
  "title"           -> addOnce k $ metaBlock k v
  "subtitle"        -> addOnce k $ metaBlock k v
  "author"          -> addOnce k $ metaList k v
  "date"            -> addOnce k $ metaBlock k v
  "publisher"       -> addOnce k $ metaBlock k v
  "keywords"        -> addOnce k $ metaList k v
  "abstract"        -> addOnce k $ metaBlock k v

  "number-sections" -> add k $ inlineFunc' "numbersections" v
  "toc-depth"       -> add k $ inlineFunc' "tocdepth" v

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

  "page-size"       -> addOnce k $ metaVar k v
  "page-div"        -> addOnce k $ metaVar k v

  "font-size"       -> addOnce k $ metaVar k v
  "main-font"       -> addOnce k $ metaVar k v
  "sans-font"       -> addOnce k $ metaVar k v
  "mono-font"       -> addOnce k $ metaVar k v

  _                 -> add "err" . pppErr $ "unknown macro " ++ k
config (Include k _) = add "err" . pppErr $ "unknown macro " ++ k

configIncArticle :: Unprocessed -> State PpP ()
configIncArticle (Markdown s)  = add "" s
configIncArticle (Macro k v)   =
  if   k `elem` [ "title", "subtitle", "author", "keywords", "abstract", "notes",
                  "notes\'", "citations", "citations\'", "bibliography",
                  "bibliography\'" ]
  then config . Macro k $ v
  else add "err" . pppErr $ "unknown or unsupported macro " ++ k ++
                            " inside an included journal article"
configIncArticle (Include k _) = add "err" . pppErr $ "unknown macro " ++ k

configJournal :: Unprocessed -> State PpP ()
configJournal (Markdown s)  = add "" s
configJournal (Macro k v)   =
  if   k `elem` [ "notes", "notes\'", "citations", "citations\'", "bibliography",
                  "bibliography\'", "lof", "lot" ]
  then add "err" . pppErr $ "macro " ++ k ++ " is unsupported for journal type"
  else config . Macro k $ v
configJournal (Include k _) = add "err" . pppErr $ "unknown macro " ++ k



renderIncArticle :: Bool -> (Int, Unprocessed) -> IO Unprocessed
renderIncArticle twocolumn (n, (Include "article" doc)) = do
  template  <- readFile =<< getDataFileName ("tex" </> "article.tex")
  let ppp    = execState (mapM_ configIncArticle doc) academicPpP{
                 writer = academicWriter{
                   writerChapters = False,
                   writerTemplate = template
                 }
               }
  pandoc <- pppToPandoc ppp twocolumn . show $ n
  return . Markdown . writeLaTeX (writer ppp) $ pandoc
renderIncArticle _ (_, x) = return x

renderJournal :: [Unprocessed] -> Bool -> FilePath -> IO ()
renderJournal doc twocolumn out = do
  template  <- readFile =<< getDataFileName ("tex" </> "report.tex")
  doc'      <- mapM (renderIncArticle twocolumn) . zip [1..] $ doc
  let ppp    = execState (mapM_ configJournal doc') academicPpP{
                 writer = academicWriter{
                   writerVariables = [
                     ("documentclass", "scrartcl"),
                     ("rootlevel", "1"),
                     ("journal", "true")
                   ] ++ if twocolumn then [("page-twocolumn", "true")] else [],
                   writerChapters = False,
                   writerTemplate = template
                 }
               }
  pandoc    <- pppToPandoc ppp twocolumn ""
  renderPDF pandoc (writer ppp) out

renderArticle :: [Unprocessed] -> Bool -> FilePath -> IO ()
renderArticle doc twocolumn out = do
  template  <- readFile =<< getDataFileName ("tex" </> "report.tex")
  let ppp    = execState (mapM_ config doc) academicPpP{
                 writer = academicWriter{
                   writerVariables = [
                     ("documentclass", "scrartcl"),
                     ("rootlevel", "1"),
                     ("article", "true")
                   ] ++ if twocolumn then [("page-twocolumn", "true")] else [],
                   writerChapters = False,
                   writerTemplate = template
                 }
               }
  pandoc    <- pppToPandoc ppp twocolumn ""
  renderPDF pandoc (writer ppp) out

renderReport :: [Unprocessed] -> Bool -> FilePath -> IO ()
renderReport doc twocolumn out = do
  template  <- readFile =<< getDataFileName ("tex" </> "report.tex")
  let ppp    = execState (mapM_ config doc) academicPpP{
                 writer = academicWriter{
                   writerVariables = [
                     ("documentclass", "scrreprt"),
                     ("rootlevel", "0"),
                     ("report", "true")
                   ] ++ if twocolumn then [("page-twocolumn", "true")] else [],
                   writerChapters = True,
                   writerTemplate = template
                 }
               }
  pandoc    <- pppToPandoc ppp twocolumn ""
  renderPDF pandoc (writer ppp) out



pppToPandoc :: PpP -> Bool -> String -> IO Pandoc
pppToPandoc ppp twocolumn prefix = do
  pandoc <- fmap toTex
          . wikiref'
          . numberRef
          . figure twocolumn
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
