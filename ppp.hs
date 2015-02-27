module Main where

import PpP.Shared
import PpP.PrePreProcess
import PpP.Renderer

import Paths_ppp
import Data.List (intercalate)
import Data.Version (showVersion)
import Control.Monad

import System.Environment
import System.Directory
import System.FilePath
import System.Exit

render :: [Unprocessed] -> FilePath -> IO ()
render doc inn = let doc' = rmType False doc in case getType doc of
  "cv"       -> renderCV doc' $ replaceExtension inn "pdf"

  "default"  -> renderReport  doc' $ replaceExtension inn "pdf"

  "report"   -> renderReport  doc' $ replaceExtension inn "pdf"
  "article"  -> renderArticle doc' $ replaceExtension inn "pdf"

  unknown    -> renderReport (rmType True doc) $ replaceExtension inn "pdf"

main :: IO ()
main = do
  args            <- getArgs
  let (flags, files) = foldl (\(a,b) x -> if take 1 x == "-" then (x:a,b) else (a,x:b)) ([],[]) args

  let help         = intercalate "\n" $ [
                       "ppp [FLAG] [FILES]",
                       "",
                       "  -v --version",
                       "  -h --help" ]
  case flags of
    ["-v"]        -> putStrLn ("ppp " ++ showVersion version)
    ["--version"] -> putStrLn ("ppp " ++ showVersion version)
    ["-h"]        -> putStrLn help
    ["--help"]    -> putStrLn help
    (x:_)         -> error $ "invalid option `" ++ x ++ "'\n" ++
                             "Try ppp --help for more info"

    []            -> do
                     when (files == []) $ putStrLn help
                     mapM_ renderFile files
                     putStrLn "done"

  exitSuccess

renderFile :: FilePath -> IO ()
renderFile fp = do
  let file = takeFileName fp

  cd  <- getCurrentDirectory
  dir <- if isRelative fp
         then do
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

  setCurrentDirectory cd
