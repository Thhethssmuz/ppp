module Main where

import PpP.Shared
import PpP.PrePreProcess
import PpP.Renderer

import System.Directory (doesFileExist)
import System.FilePath
import System.Exit

render :: [Unprocessed] -> FilePath -> IO ()
render doc inn = case getType doc of
  "report"  -> renderReport (filterType doc) $ replaceExtension inn "pdf"
  "default" -> renderReport (filterType doc) $ replaceExtension inn "pdf"
  unknown   -> renderReport (
                   filterType
                 . replaceType (Markdown $ pppErr [("type", unknown)])
                 $ doc
               ) $ replaceExtension inn "pdf"

main :: IO ()
main = do
  let fp = "test/test.md"

  exist <- doesFileExist fp
  if exist
  then return ()
  else do
       putStrLn $ "ppp: unable to render " ++ fp ++ ", file not found"
       exitFailure

  doc <- prePreProcess fp
  render doc fp

  putStrLn "done"
  exitSuccess
