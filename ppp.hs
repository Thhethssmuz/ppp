module Main where

import PpP.Shared
import PpP.PrePreProcess
import PpP.Renderer

import System.Environment
import System.Directory
import System.FilePath
import System.Exit

render :: [Unprocessed] -> FilePath -> IO ()
render doc inn = case getType doc of
  "report"  -> renderReport (rmType False doc) $ replaceExtension inn "pdf"
  "default" -> renderReport (rmType False doc) $ replaceExtension inn "pdf"
  unknown   -> renderReport (rmType True doc) $ replaceExtension inn "pdf"

main :: IO ()
main = do
  args <- getArgs

  case args of 
    [] -> do
          putStrLn $ "ppp: called with no arguments"
          exitFailure
    _  -> return ()

  let fp = head args
  let file = takeFileName fp
  dir <- if isRelative fp then do
                               cd <- getCurrentDirectory
                               return . combine cd . dropFileName $ fp
                          else return . dropFileName $ fp

  setCurrentDirectory . dropFileName $ fp

  exist <- doesFileExist file
  if exist
  then return ()
  else do
       putStrLn $ "ppp: unable to render " ++ fp ++ ", file not found"
       exitFailure

  doc <- prePreProcess file
  render doc file

  putStrLn "done"
  exitSuccess
