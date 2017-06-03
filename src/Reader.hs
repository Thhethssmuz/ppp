module Reader (reader, toPandoc) where

import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Options (ReaderOptions(..), def)

reader :: ReaderOptions
reader = def
  { readerSmart      = True
  , readerStandalone = True
  , readerParseRaw   = True
  }

toPandoc = readMarkdown reader
