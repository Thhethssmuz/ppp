module PreProcess (include) where

import Control.Monad (when)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, groupBy)
import System.Directory (doesFileExist)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

unlines' :: [String] -> String
unlines' = intercalate "\n"

isMacro (x:xs) = x == '%'
isMacro _      = False

hasIndent _ []     = True
hasIndent _ (y:ys) = isSpace y

groupBlocks :: String -> [String]
groupBlocks
  = map unlines'
  . groupBy (\x y -> not (isMacro x || isMacro y))
  . map unlines'
  . groupBy hasIndent
  . lines

processMacro :: String -> IO String
processMacro block = do
  let block' = drop 1 block
      macro  = map toLower . trim . takeWhile (/= ':') $ block'
      inner  = trim . dropWhile (== ':') . dropWhile (/= ':') $ block'

  if macro == "include"
    then fmap (concatMap ("\n\n" ++)) . mapM (include . trim) . lines $ inner
    else return $ "\n\n<div ppp=\"" ++ macro ++ "\">" ++ inner ++ "</div>\n\n"

processMacros :: [String] -> IO String
processMacros [] = return ""
processMacros (x:xs)
  | isMacro x = do
                x'  <- processMacro x
                xs' <- processMacros xs
                return $ x' ++ xs'
  | otherwise = fmap ((++) x) $ processMacros xs

include :: FilePath -> IO String
include fp = do
  e <- doesFileExist fp
  when (not e) . error $ "file " ++ fp ++ " not found"
  file <- readFile fp
  processMacros . groupBlocks . filter (/= '\r') $ file
