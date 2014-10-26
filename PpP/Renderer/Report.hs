module PpP.Renderer.Report (renderReport) where

import PpP.Filter
import PpP.Shared
import PpP.Renderer.Shared

import Text.Pandoc
import Text.Pandoc.PDF
import Text.Pandoc.Walk
import Text.Highlighting.Kate.Styles
import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char (toLower)

import System.Exit


configure :: Unprocessed -> State PpP ()
configure (Markdown s) = add "" s
configure (Macro k v)  = case k of

  "header"          -> let hs = parseList v in
                       case length hs of
                         0 -> return ()
                         1 -> addOnce k $ metaVar "page-header-centre" (head hs)
                         2 -> addOnce k $ metaVar "page-header-left"   (head hs) ++
                                          metaVar "page-header-right"  (last hs)
                         3 -> addOnce k $ metaVar "page-header-left"   (head hs) ++
                                          metaVar "page-header-centre" (hs !! 1) ++
                                          metaVar "page-header-right"  (last hs)
                         _ -> do add "err" $ pppErr [("tomanyargs", k)]
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
                         _ -> do add "err" $ pppErr [("tomanyargs", k)]
                                 configure . Macro k . unlines . take 3 $ fs

  "subject"         -> addOnce k $ metaBlock k v
  "title"           -> addOnce k $ metaBlock k v
  "author"          -> addOnce k $ metaList k v
  "date"            -> addOnce k $ metaBlock k v
  "publisher"       -> addOnce k $ metaBlock k v
  "keywords"        -> addOnce k $ metaList k v
  "abstract"        -> addOnce k $ inlineFunc k ""

  "toc"             -> addOnce k $ metaBlock k v ++ inlineFunc k ""
  "lof"             -> addOnce k $ inlineFunc k ""
  "lot"             -> addOnce k $ inlineFunc k ""

  _                 -> add "err" . pppErr $ [("unknown", k)]


renderReport :: [Unprocessed] -> FilePath -> IO ()
renderReport doc out = do
  template <- readFile "tex/report.tex"

  let ppp   = execState (mapM_ configure doc) emptyPpP{
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

  pandoc   <- wikiref'
            . readMarkdown (reader ppp)
            $ document ppp

  printErrors pandoc

  putStrLn $ "rendering " ++ out


  {-
  pdf <- makePDF "xelatex" writeLaTeX (writer ppp) pandoc

  case pdf of
    Left err -> do
                BS.putStrLn err
                exitFailure
    Right bs -> BS.writeFile out bs
  -}
