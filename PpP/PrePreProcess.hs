module PpP.PrePreProcess where

import Text.ParserCombinators.Parsec

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import System.Directory (doesFileExist)

import Data.Maybe (fromJust)
import Data.List (intersperse, elemIndex, groupBy)
import Data.Char (isSpace, toLower)
import Data.List.Split (splitOn)



data Unprocessed = Markdown String
                 | Macro String String
                 deriving (Show)

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



trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

parseList :: String -> [String]
parseList = filter (not . null) . map trim . concatMap lines . splitOn ";"



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
  Left err -> do
              lift . putStrLn $ "ppp: failed to parse macro \n" ++ trim s
              lift . putStrLn . concat . intersperse " " . lines . show $ err
              lift . putStrLn $ ""
  Right dp -> addUnprocessed dp

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
  ex <- lift . doesFileExist $ fp
  if   not ex
  then lift . putStrLn $ "ppp: file " ++ fp ++ " does not exist"
  else do 
    file <- lift . readFile $ fp
    modify (++ [Markdown "\n\n"])
    mapM_ parseUnprocessed . groupUnprocessed $ file
    modify (++ [Markdown "\n\n"])

getType :: [Unprocessed] -> String
getType ((Macro "type" x):xs) = map toLower x
getType (x:xs) = getType xs
getType [] = "%\n%" -- just something that is unparsable

prePreProcess :: FilePath -> IO [Unprocessed]
prePreProcess fp = execStateT (includeFile fp) []
