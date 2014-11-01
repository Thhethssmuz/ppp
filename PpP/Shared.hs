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


pppErr :: String -> String
pppErr err = "\n\n<div class=\"ppp-err\" err=\"" ++ err ++ "\"></div>\n\n"

inlineFunc :: String -> String -> String
inlineFunc name block = 
  "\n\n<div class=\"ppp-fn\" name=\""++name++"\">\n"++block++"\n</div>\n\n"

metaVar :: String -> String -> String
metaVar name value = "\n\n---\n" ++ name ++ ": " ++ value ++ "\n---\n\n"

metaList :: String -> String -> String
metaList name list = metaVar name . concatMap ("\n- " ++) . parseList $ list

metaBlock :: String -> String -> String
metaBlock name block = "\n\n---\n" ++ name ++ ": |\n  " ++ block ++ "\n---\n\n"
