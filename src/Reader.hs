module Reader (reader, toPandoc) where

import qualified Data.Text as T
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown (readMarkdown)

reader :: ReaderOptions
reader = def
  { readerStandalone = True
  , readerExtensions = disableExtension Ext_pandoc_title_block
                     . disableExtension Ext_yaml_metadata_block
                     . disableExtension Ext_table_captions
                     $ pandocExtensions
  }

toPandoc :: String -> IO Pandoc
toPandoc = runIOorExplode . readMarkdown reader . T.pack
