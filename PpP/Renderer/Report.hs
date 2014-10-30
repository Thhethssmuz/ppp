module PpP.Renderer.Report (renderReport) where

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


configure :: Unprocessed -> State PpP ()
configure (Markdown s) = add "" s
configure (Macro k v)  = case k of

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
                              configure . Macro k . unlines . take 3 $ hs

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
                              configure . Macro k . unlines . take 3 $ fs

  "subject"         -> addOnce k $ metaBlock k v
  "title"           -> addOnce k $ metaBlock k v
  "subtitle"        -> addOnce k $ metaBlock k v
  "author"          -> addOnce k $ metaList k v
  "date"            -> addOnce k $ metaBlock k v
  "publisher"       -> addOnce k $ metaBlock k v
  "keywords"        -> addOnce k $ metaList k v
  "abstract"        -> addOnce k $ inlineFunc k ""

  "toc"             -> addOnce k $ -- metaVar k "true" ++ 
                                   inlineFunc k ""
  "lof"             -> addOnce k $ inlineFunc k ""
  "lot"             -> addOnce k $ inlineFunc k ""

  "appendices"      -> addOnce' k $ inlineFunc k ""

  "bibliography"    -> addOnce k $ metaVar k v ++ inlineFunc k ""

  "csl"             -> addOnce k $ metaVar k v

  "notes"           -> let v' = map toLower v in
                       addOnce k $ inlineFunc k "" ++
                       case v' of
                         "simple"  -> metaVar "notes-chapter" "true"
                         "grouped" -> metaVar "notes-chapter" "true" ++
                                      metaVar "grouped-notes" "true"
                         "wikiref" -> metaVar "wikiref" "true"
                         ""        -> metaVar "wikiref" "true"
                         _         -> metaVar "wikiref" "true" ++
                                      pppErr ("unknown argument " ++ v' ++
                                              " applied to macro notes")

  _                 -> add "err" . pppErr $ "unknown macro " ++ k


renderReport :: [Unprocessed] -> FilePath -> IO ()
renderReport doc out = do
  template  <- readFile =<< getDataFileName ("tex" </> "report.tex")

  let ppp    = execState (mapM_ configure doc) emptyPpP{
                 reader = def{
                   readerSmart = True,
                   readerStandalone = True,
                   readerParseRaw = True
                 },
                 writer = def{
                   writerStandalone = True,
                   writerHighlight = True,
                   writerHighlightStyle = tango,
                   writerChapters = True,
                   writerTemplate=template
                 }
               }

  pandoc    <- fmap toTex
             . wikiref'
             . readMarkdown (reader ppp)
             $ document ppp

  printErrors pandoc

  putStrLn $ "rendering " ++ out

  pdf <- makePDF "xelatex" writeLaTeX (writer ppp) pandoc

  case pdf of
    Left err -> do
                BS.putStrLn err
                exitFailure
    Right bs -> BS.writeFile out bs
