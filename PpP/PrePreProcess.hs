module PpP.PrePreProcess where

import PpP.Shared

import Text.ParserCombinators.Parsec
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import System.Directory (doesFileExist)
import Data.List (intersperse, groupBy)
import Data.Char (isSpace, toLower)


wordChar = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "'-_"

word :: Parser String
word = many1 $ oneOf wordChar

content :: Parser String
content = do
  char ':'
  manyTill anyChar eof

macro :: Parser Unprocessed
macro = do
  char '%'
  spaces
  n  <- word
  spaces
  c  <- content <|> do
                    spaces
                    eof
                    return ""
  return $ Macro (map toLower n) (trim c)

markdown :: Parser Unprocessed
markdown = do
  str <- manyTill anyChar eof
  return $ Markdown str

unprocessed :: Parser Unprocessed
unprocessed = macro <|> markdown


unlines' :: [String] -> String
unlines' = foldl1 (\x y -> x++'\n':y)

groupUnprocessed :: String -> [String]
groupUnprocessed =
  let f _ []     = True
      f _ (y:ys) = isSpace y
      g (x:xs) = x /= '%'
      g _      = True
  in  map unlines'
    . groupBy (\x y -> g x && g y)
    . map unlines'
    . groupBy f
    . lines


parseUnprocessed :: String -> StateT [Unprocessed] IO ()
parseUnprocessed s = case parse unprocessed "" s of
  Left err -> addUnprocessed . Markdown . pppErr $ 
              "unable to parse macro " ++ trim s ++ "\n" ++ prettify err ++ "\n"

  Right dp -> addUnprocessed dp

  where prettify = concat . intersperse "\n  " . lines . show

addUnprocessed :: Unprocessed -> StateT [Unprocessed] IO ()
addUnprocessed (Macro k v) = case k of
  "include"    -> mapM_ includeFile . parseList $ v
  "appendix"   -> addUnprocessed $ Macro "appendices" v
  "appendices" -> do
                  modify (++ [Macro k ""])
                  mapM_ includeFile . parseList $ v
  _            -> modify (++ [Macro k v])
addUnprocessed dp = modify (++ [dp])


includeFile :: FilePath -> StateT [Unprocessed] IO ()
includeFile fp = do
  exist <- lift . doesFileExist $ fp

  if   not exist
  then addUnprocessed . Markdown . pppErr $ "file " ++ fp ++ " not found"
  else do 
       file <- lift . readFile $ fp
       modify (++ [Markdown "\n\n"])
       mapM_ parseUnprocessed . groupUnprocessed $ file
       modify (++ [Markdown "\n\n"])

prePreProcess :: FilePath -> IO [Unprocessed]
prePreProcess fp = execStateT (includeFile fp) []


getType :: [Unprocessed] -> String
getType ((Macro "type" x):xs) = map toLower x
getType (x:xs) = getType xs
getType [] = "default"

rmType :: Bool -> [Unprocessed] -> [Unprocessed]
rmType unknown ((Macro "type" x):xs) = err ++ map mi xs
  where err = if unknown then [Markdown . pppErr $ "unknown document type " ++ x]
                         else []
        mi (Macro "type" _) = Markdown . pppErr $ "multiple instances of macro type"
        mi x = x
rmType unknown (x:xs) = x : rmType unknown xs
rmType _ [] = []
