module PpP.Renderer.Shared where

import PpP.Shared

import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Lazy as M
import Text.Pandoc (ReaderOptions, WriterOptions, def)


data PpP = PpP {
  document :: String,
  keycount :: M.Map String Int,
  reader   :: ReaderOptions,
  writer   :: WriterOptions
}

emptyPpP :: PpP
emptyPpP = PpP "" (M.fromList []) def def

counter :: String -> State PpP Int
counter k = gets (fromMaybe 0 . M.lookup k . keycount)

increment :: String -> State PpP ()
increment k = do
  c <- counter k
  modify (\doc -> doc{keycount = M.insert k (c+1) $ keycount doc})

add :: String -> String -> State PpP ()
add k v = do
  increment k
  modify (\doc -> doc{document = (document doc) ++ v})

addOnce :: String -> String -> State PpP ()
addOnce k v = do
  c <- counter k
  if c > 0 then add "err" $ pppErr [("multiinstance", k)]
           else add k v

addOnce' :: String -> String -> State PpP ()
addOnce' k v = do
  c <- counter k
  if c > 0 then return () else add k v


