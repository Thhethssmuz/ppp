module PpP.Renderer.Report (renderReport) where

import PpP.Filter
import PpP.PrePreProcess
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


configure :: Unprocessed -> StateT PpP IO ()
configure (Markdown s) = add "" s
configure (Macro k v)  = case k of
  "type"            -> return ()

  _              -> lift . putStrLn $ "ppp: unknown macro " ++ k


renderReport :: [Unprocessed] -> FilePath -> IO ()
renderReport doc out = do
  template <- readFile "tex/report.tex"

  ppp      <- execStateT (mapM_ configure doc) emptyPpP{
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


  putStrLn $ "rendering " ++ out


  pdf <- makePDF "xelatex" writeLaTeX (writer ppp) pandoc

  case pdf of
    Left err -> do
                BS.putStrLn err
                exitFailure
    Right bs -> BS.writeFile out bs
