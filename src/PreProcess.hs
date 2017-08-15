module PreProcess (include) where

import Control.Monad (when)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, groupBy)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import Text.Pandoc.Shared (splitBy)

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
  let block'  = drop 1 block
      macro   = map toLower . trim . takeWhile (/= ':') $ block'
      inner   = (\xs -> if null xs then Nothing else Just $ tail xs)
              . dropWhile (/= ':') $ block'

      raw     = fromMaybe "" . fmap ((" ppp-raw="++) . show) $ inner
      inlines = fromMaybe ""
              . fmap ( ("<span class=\"ppp-inlines\">\n"++)
                     . (++"</span>\n")
                     . concatMap (("<span>"++) . (++"</span>\n"))
                     . filter (not . null)
                     . map trim
                     . splitBy (== '\n')
                     ) $ inner
      blocks  = fromMaybe ""
              . fmap ( ("<div class=\"ppp-blocks\">\n"++)
                     . (++"</div>\n")
                     ) $ inner

  if macro == "include"
    then fmap (concatMap ("\n\n" ++)) . mapM (include . trim) . lines . fromMaybe "" $ inner
    else return $ "\n\n<div ppp=" ++ (show macro) ++ raw ++ ">\n" ++
                  inlines ++ blocks ++ "</div>\n\n"

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
