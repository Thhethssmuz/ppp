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
  "type"            -> return ()

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
