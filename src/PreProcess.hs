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
processMacro macroBlock = do
  let block  = drop 1 macroBlock

      macro   = map toLower . trim . takeWhile (/= ':') $ block
      content = (\xs -> if null xs then Nothing else Just . trim $ tail xs)
              . dropWhile (/= ':') $ block

      lines   = fromMaybe []
              . fmap (map trim . splitBy (flip elem "\n;"))
              $ content

      cs      = ["ppp-macro"]
      as      = [("macro", macro)] ++
                zipWith (\l i -> ("l" ++ show i, l)) lines [0..]

      attr    = "{" ++
                (intercalate " " $ map (('.':)) cs) ++ " " ++
                (intercalate " " $ map (\(k,v) -> k ++ "=" ++ show v) as) ++
                "}"
      inner   = concat
              . zipWith (\l i -> "\n[" ++ l ++ "]{.line}") lines
              $ [0..]

  if macro == "include"
    then fmap ((++"\n\n") . concatMap ("\n\n"++)) . mapM include $ lines
    else return $ "\n\n:::" ++ attr ++ inner ++ "\n:::\n\n"

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
