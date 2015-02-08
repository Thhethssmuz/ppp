module PpP.Renderer.Academic (renderReport, renderArticle) where

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
import Data.Maybe
import Data.List (intersperse)

import Paths_ppp
import System.FilePath
import System.Exit

-------------------------------------------------------------------------------

config :: Unprocessed -> StateT PpP IO ()
config (Markdown s)  = add "" s
config (Macro k v)   = case k of

  "language"        -> let lang = map toLower v
                           loc  = M.lookup lang languages
                       in case loc of
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

  "page"            -> forM_ (map (map toLower) . parseList $ v) $ \x ->
                         case x of
                           "twoside"   -> addOnce x $ metaVar "page-twoside" "true"
                           _           -> add "err" . pppErr $ "unknown argument '" ++ x ++ "' applied to page macro"
  "pagecols"        -> unless (v == "1") . addOnce k $ metaVar "page-columns" v
  "pagesize"        -> addOnce k $ metaVar "page-size" v
  "pagediv"         -> addOnce k $ metaVar "page-div" v
  "pagebcor"        -> addOnce k $ metaVar "page-bcor" v

  "fontsize"        -> addOnce k $ metaVar "font-size" v
  "fontmain"        -> addOnce k $ metaVar "main-font" v
  "fontsans"        -> addOnce k $ metaVar "sans-font" v
  "fontmono"        -> addOnce k $ metaVar "mono-font" v
  "fontmath"        -> addOnce k $ metaVar "math-font" v

  "titlehead"       -> let xs = parseList v in case length xs of
                         0 -> return ()
                         1 -> addOnce k $ metaBlock "title-head-centre" (head xs)
                         2 -> addOnce k $ metaBlock "title-head-left"   (head xs) ++
                                          metaBlock "title-head-right"  (last xs)
                         3 -> addOnce k $ metaBlock "title-head-left"   (head xs) ++
                                          metaBlock "title-head-centre" (xs !! 1) ++
                                          metaBlock "title-head-right"  (last xs)
                         _ -> do
                              add "err" . pppErr $ "to many arguments applied to macro " ++ k
                              config . Macro k . unlines . take 3 $ xs

  "subject"         -> addOnce k $ metaBlock k v
  "title"           -> addOnce k $ metaBlock k v
  "subtitle"        -> addOnce k $ metaBlock k v
  "author"          -> addOnce k $ metaList k v
  "date"            -> addOnce k $ metaBlock k v
  "publisher"       -> addOnce k $ metaBlock k v
  "keywords"        -> addOnce k $ metaList k v
  "abstract"        -> addOnce k $ metaBlock k v

  "tocdepth"        -> add k $ inlineFunc' k v
  "numdepth"        -> do
                       m <- counter "mainmatter"
                       if m > 0
                       then add k $ inlineFunc' k v
                       else setCounter "atmainmatter-numdepth"
                          . maybe (-2) fst . listToMaybe $ reads v

  "tableofcontents" -> do
                       addOnce k $ inlineFunc  ("ppp" ++ k) ""
                       d <- counter "tocdepth"
                       unless (d > 0) . add "tocdepth" . raw $ "\\tocdepth{3}"
  "listoffigures"   -> addOnce k $ inlineFunc' ("ppp" ++ "listof") "figure"
  "listoftables"    -> addOnce k $ inlineFunc' ("ppp" ++ "listof") "table"
  "listofprograms"  -> addOnce k $ inlineFunc' ("ppp" ++ "listof") "program"
  "listofexamples"  -> addOnce k $ inlineFunc' ("ppp" ++ "listof") "example"

  "csl"             -> addOnce k $ metaVar k v

  "notes"           -> let v' = map toLower v in
                       addOnce k $ inlineFunc "notes" "" ++
                       case v' of
                         "simple"  -> metaVar "notes-chapter" "true"
                         "grouped" -> metaVar "notes-chapter" "true" ++
                                      metaVar "grouped-notes" "true"
                         ""        -> metaVar "notes" "true"
                         _         -> metaVar "notes" "true" ++
                                      pppErr ("unknown argument " ++ v' ++
                                              " applied to macro notes")

  "bibliography"    -> do
                       addOnce k $ metaList k v ++
                                   func "begin" "pppbibliography" ++
                                   "\n\n<div insert=\"bibliography\"></div>\n\n" ++
                                   func "end" "pppbibliography"

  "bib-flush-hack"  -> add k $ metaVar k "true"
  "linksasnotes"    -> addOnce k $ metaVar "links-as-notes" "true"

  "mainmatter"      -> do
                       c <- counter "numdepth"
                       d <- counter "atmainmatter-numdepth"
                       addOnce k . raw $ "\\numdepth{3}"
                       when (c > 0) . add "numdepth" . func "numdepth" $ show d

  "backmatter"      -> do
                       a <- counter "appendix"
                       addOnce k . raw . concat . intersperse "\n" $
                         (if a > 0 then [
                         "\\end{pppmulticol}",
                         "\\makeatletter",
                         "\\renewcommand{\\toclevel@part}{-1}",
                         "\\renewcommand{\\toclevel@chapter}{0}",
                         "\\renewcommand{\\toclevel@section}{1}",
                         "\\renewcommand{\\toclevel@subsection}{2}",
                         "\\renewcommand{\\toclevel@subsubsection}{3}",
                         "\\renewcommand{\\toclevel@paragraph}{4}",
                         "\\renewcommand{\\toclevel@subparagraph}{5}",
                         "\\makeatother",
                         "\\begin{pppmulticol}" ] else []) ++ [
                         "\\numdepth{-1}",
                         "\\renewcommand{\\numdepth}[1]{}" ]

  _                 -> add "err" . pppErr $ "unknown macro " ++ k

config (Include k v) = case k of
  "appendix"        -> do
                       addOnce' k $ raw . concat . intersperse "\n" $ [
                         "\\end{pppmulticol}",
                         "\\pppclear",
                         "\\part*{\\appendixpagename}",
                         "\\appendix",
                         "\\addcontentsline{toc}{chapter}{\\appendixpagename}",
                         "",
                         "\\makeatletter",
                         "\\renewcommand{\\toclevel@part}{0}",
                         "\\renewcommand{\\toclevel@chapter}{1}",
                         "\\renewcommand{\\toclevel@section}{2}",
                         "\\renewcommand{\\toclevel@subsection}{3}",
                         "\\renewcommand{\\toclevel@subsubsection}{4}",
                         "\\renewcommand{\\toclevel@paragraph}{5}",
                         "\\renewcommand{\\toclevel@subparagraph}{6}",
                         "\\makeatother",
                         "\\begin{pppmulticol}"
                         ]
                       forM_ v config

  _                 -> add "err" . pppErr $ "unknown macro " ++ k

-------------------------------------------------------------------------------

renderArticle :: [Unprocessed] -> FilePath -> IO ()
renderArticle doc out = do
  template  <- readFile =<< getDataFileName ("tex" </> "report.tex")
  ppp       <- execStateT (mapM_ config doc) academicPpP{
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
  ppp       <- execStateT (mapM_ config doc) academicPpP{
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

-------------------------------------------------------------------------------

pppToPandoc :: PpP -> String -> IO Pandoc
pppToPandoc ppp prefix = do
  let m = writerChapters . writer $ ppp

  pandoc <- fmap toTex
          . reference'
          . numberRef
          . float
          . multicol m
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
