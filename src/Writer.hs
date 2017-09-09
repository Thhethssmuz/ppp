module Writer (writer, toTex, toPdf) where

import Paths_ppp (version)
import Emb (emb)

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Skylighting.Styles (tango)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Pandoc.Definition (Pandoc(..), MetaValue(..), lookupMeta)
import Text.Pandoc.Options (WriterOptions(..), TopLevelDivision(..), def)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Writers.LaTeX (writeLaTeX)

writer :: WriterOptions
writer = def
  { writerTemplate         = fmap (BS.unpack . BS.fromStrict)
                           . lookup "template.tex" $ emb
  , writerHighlight        = True
  , writerHighlightStyle   = tango
  , writerVariables        = [ ("ppp-version", showVersion version) ]
  }

extendWriterOptions :: WriterOptions -> Pandoc -> WriterOptions
extendWriterOptions opts (Pandoc meta _) =
  let dc = lookupMeta "documentclass" meta
      tl = if dc == (Just $ MetaString "scrartcl")
           then TopLevelSection
           else TopLevelChapter
  in  opts { writerTopLevelDivision = tl }

toTex :: Pandoc -> String
toTex doc = writeLaTeX (extendWriterOptions writer doc) doc

toPdf :: Pandoc -> IO BS.ByteString
toPdf doc = do
  let writer' = extendWriterOptions writer doc
  pdf <- makePDF "xelatex" writeLaTeX writer' doc
  case pdf of
    Left err -> do
                BS.hPutStrLn stderr err
                exitFailure
    Right bs -> return bs
