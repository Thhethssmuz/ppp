module PpP.Shared where

import Text.Pandoc.Shared (splitBy)
import Data.Char (isSpace)


data Unprocessed = Markdown String
                 | Macro String String
                 deriving (Show)


trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

parseList :: String -> [String]
parseList = filter (not . null) . map trim . concatMap lines . splitBy (==';')


pppErr :: [(String, String)] -> String
pppErr kv = "\n<div class=\"ppp-err\"" ++ attrs ++ "></div>\n\n"
  where attrs = concatMap (\(k,v) -> " " ++ k ++ "=\"" ++ v ++ "\"") kv

pppFunc :: String -> String -> String
pppFunc name block = 
  "\n<div class=\"ppp-fn\" name=\""++name++"\">\n"++block++"\n</div>\n\n"

pppVar :: String -> [String] -> String
pppVar name args = "\n---\n" ++ name ++ ": " ++ format (length args) ++ "\n---\n\n"
  where format 0 = "false"
        format 1 = head args
        format _ = concatMap ("\n- " ++) args

pppVar' :: String -> String -> String
pppVar' name block = "\n---\n" ++ name ++ ": |\n  " ++ block ++ "\n---\n\n"
