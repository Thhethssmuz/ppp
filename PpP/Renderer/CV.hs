module PpP.Renderer.CV (renderCV) where

import PpP.Err
import PpP.Filter
import PpP.Shared
import PpP.Language
import PpP.Renderer.Shared

import Text.Pandoc
import Text.Pandoc.PDF
import Text.Pandoc.Walk
import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Ord (comparing)
import Data.Char (toLower)
import Data.List (transpose, sortBy)
import Data.Maybe (fromMaybe, isJust)

import Paths_ppp
import System.FilePath
import System.Exit


configure :: Unprocessed -> StateT PpP IO ()
configure (Markdown s) = add "" s
configure (Macro k v)  = case k of

  "author"          -> addOnce k $ metaVar k v
  "address"         -> addOnce k . metaVar k . concat 
                     $ ["\naddress" ++ show i ++ ": " ++ x | (i,x) <- zip [1..] (parseList v)]
  "phone"           -> addOnce k $ metaVar k v
  "email"           -> addOnce k $ metaVar k v
  "webpage"         -> addOnce k $ metaVar k v

  "page-size"       -> addOnce k $ metaVar k v
  "page-div"        -> addOnce k $ metaVar k v
  "page-numbers"    -> addOnce k $ metaVar k "true"

  "font-size"       -> addOnce k $ metaVar k v
  "main-font"       -> addOnce k $ metaVar k v
  "sans-font"       -> addOnce k $ metaVar k v
  "mono-font"       -> addOnce k $ metaVar k v

  _                 -> add "err" . pppErr $ "unknown macro " ++ k

tableHack :: Block -> Block
tableHack (Table is as (w:x:[]) hs tcs) = Table is as [0.66, 0.33 + 0.05] hs tcs
tableHack x = x

renderCV :: [Unprocessed] -> FilePath -> IO ()
renderCV doc out = do
  template  <- readFile =<< getDataFileName ("tex" </> "cv.tex")

  ppp       <- execStateT (mapM_ configure doc) emptyPpP{
                 reader = def{
                   readerSmart = True,
                   readerStandalone = True,
                   readerParseRaw = True
                 },
                 writer = def{
                   writerStandalone = True,
                   writerChapters = False,
                   writerTemplate = template
                 }
               }

  let pandoc = walk tableHack
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
