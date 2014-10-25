module Main where

import PpP.PrePreProcess
import PpP.Renderer

import System.FilePath.Posix
import System.Directory (doesFileExist)
import System.Exit

render :: [Unprocessed] -> FilePath -> IO ()
render doc inn = case getType doc of
  "report" -> renderReport doc $ replaceExtension inn "pdf"
  "%\n%"   -> renderReport doc $ replaceExtension inn "pdf"
  other    -> do
              putStrLn $ "ppp: unknown document type " ++ other
              renderReport doc $ replaceExtension inn "pdf"

main :: IO ()
main = do
  let fp = "test/test.md"

  ex <- doesFileExist fp
  case ex of
    False -> do
             putStrLn $ "ppp: file " ++ fp ++ " does not exist"
             exitFailure
    True  -> return ()

  doc <- prePreProcess fp
  render doc fp

  putStrLn "done"
  exitSuccess
