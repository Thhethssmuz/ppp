module BlockWriter where

import Control.Monad.Trans.State.Lazy
import Text.Pandoc.Definition

-- Writer monad for [Block] that concatenates adjacent Plain blocks
type BlockWriter = State [Block] ()

inlines :: [Inline] -> BlockWriter
inlines is = modify $ \bs -> case bs of
  (Plain js):bs -> (Plain $ js ++ is) : bs
  bs            -> (Plain is) : bs

blocks :: [Block] -> BlockWriter
blocks [] = return ()
blocks ((Plain is):xs) = do
  modify $ \bs -> case bs of
    (Plain js):bs -> (Plain $ js ++ is) : bs
    bs            -> (Plain is) : bs
  blocks xs
blocks (x:xs) = do
  modify $ (x:)
  blocks xs

tex :: String -> BlockWriter
tex = inline . RawInline (Format "tex") . (++"\n")

inline :: Inline -> BlockWriter
inline = inlines . (:[])

block :: Block -> BlockWriter
block = blocks . (:[])

runBlockWriter :: BlockWriter -> [Block] -> [Block]
runBlockWriter f = reverse . execState f . reverse
