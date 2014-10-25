module PpP.PrePreProcess where

import PpP.Shared

import Text.ParserCombinators.Parsec
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import System.Directory (doesFileExist)
import Data.List (intersperse, groupBy)
import Data.Char (isSpace, toLower)


wordChar = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "-_"

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
              [("macro", trim s), ("err", prettify err)]

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
  then addUnprocessed . Markdown . pppErr $ [("file", fp)]
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

replaceType :: Unprocessed -> [Unprocessed] -> [Unprocessed]
replaceType r ((Macro "type" _):xs) = r:xs
replaceType r (x:xs) = x : replaceType r xs
replaceType _ [] = []

filterType :: [Unprocessed] -> [Unprocessed]
filterType = filter f
  where f (Macro "type" _) = False
        f _ = True
