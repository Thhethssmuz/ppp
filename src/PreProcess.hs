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
      inner   = fmap (map trim . splitBy (flip elem "\n;") . trim)
              . (\xs -> if null xs then Nothing else Just $ tail xs)
              . dropWhile (/= ':') $ block'
      raw     = fromMaybe ""
              . fmap ( (" ppp-raw=\""++) . (++"\"") . intercalate "\n"
                     . filter (not . null) . map (tail . init . show)
                     )
              $ inner

      content = fromMaybe "" . flip fmap inner $ \xs ->
                  let is = concatMap (("<span class=\"ppp-line\">"++) . (++"\\\n</span>"))
                         . (\xs -> if null xs then [""] else xs)
                         . filter (not . null) $ xs
                      b  = ("<span class=\"ppp-multi\">"++) . (++"\\\n</span>")
                         . intercalate "\\\n" $ xs
                  in  is ++ b

  if macro == "include"
    then fmap (concatMap ("\n\n" ++)) . mapM (include . trim) . fromMaybe [] $ inner
    else return $ "\n\n<div ppp=" ++ (show macro) ++ raw ++ ">" ++ content ++ "</div>\n\n"

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
