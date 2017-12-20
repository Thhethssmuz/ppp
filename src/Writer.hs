module Writer (writer, toTex, toPdf) where

import Paths_ppp (version)
import Emb (emb)

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text)
import Data.Version (showVersion)
import Skylighting.Styles (tango)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Definition (Pandoc(..), MetaValue(..), lookupMeta, nullMeta)
import Text.Pandoc.Options (WriterOptions(..), TopLevelDivision(..), def)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Writers.LaTeX (writeLaTeX)

writer :: WriterOptions
writer = def
  { writerTemplate         = fmap (BS.unpack . BS.fromStrict)
                           . lookup "template.tex" $ emb
  , writerHighlightStyle   = Just tango
  , writerVariables        = [ ("ppp-version", showVersion version) ]
  }

extendWriterOptions :: WriterOptions -> Pandoc -> WriterOptions
extendWriterOptions opts (Pandoc meta _) =
  let dc = lookupMeta "documentclass" meta
      tl = if dc == (Just $ MetaString "scrartcl")
           then TopLevelSection
           else TopLevelChapter
  in  opts { writerTopLevelDivision = tl }

toTex :: Pandoc -> IO Text
toTex doc = runIOorExplode $ writeLaTeX (extendWriterOptions writer doc) doc

toPdf :: Text -> IO BS.ByteString
toPdf tex = do
  let doc = Pandoc nullMeta []
  pdf <- runIOorExplode $ makePDF "xelatex" [] (\_ _ -> return tex) writer doc
  case pdf of
    Left err -> do
                BS.hPutStrLn stderr err
                exitFailure
    Right bs -> return bs
