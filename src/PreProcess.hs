module PreProcess (include) where

import Reader

import Control.Monad (when)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, groupBy)
import System.Directory (doesFileExist)
import Text.HTML.TagSoup (Tag(..), renderTags)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

unlines' :: [String] -> String
unlines' = intercalate "\n"

lines' :: String -> [String]
lines' = foldr f [] . lines
  where
    f y (x:xs) | not (null y) && last y == '\\' = (y ++ '\n':x) : xs
               | otherwise = y : (x:xs)
    f y xs     = y:xs

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
processMacro macroBlock = do
  let block    = trim $ drop 1 macroBlock
      isSep c  = c == ':' || isSpace c
      notSep   = not . isSep

      macro    = takeWhile notSep $ block
      sep      = length . takeWhile (== ':') . dropWhile isSpace
               . dropWhile notSep $ block
      content  = dropWhile isSpace . dropWhile (== ':')
               . dropWhile isSpace . dropWhile notSep $ block

      rawLines = lines' $ content
      indent   = foldl min 80 . map (length . takeWhile isSpace) . drop 1
               . filter (not . all isSpace) $ rawLines
      lines    = take 1 rawLines ++ map (drop indent) (drop 1 rawLines)
      trimmed  = filter (not . null) . map trim $ lines

      attr     = renderTags . (:[TagClose "div"]) . TagOpen "div" $
                 [ ("class", "ppp-macro")
                 , ("macro", macro)
                 , ("sep", show sep)
                 , ("lines", show $ length lines)
                 ] ++ zipWith (\l i -> ("l" ++ show i, l)) lines [0..]

  if map toLower macro == "include"
    then fmap ((++"\n\n") . concatMap ("\n\n"++)) . mapM include $ trimmed
    else return $ "\n\n" ++ attr ++ "\n\n"


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
