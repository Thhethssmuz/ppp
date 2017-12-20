module Main where

import Filter.Abstract (abstract)
import Filter.Attr (simplifyAttr)
import Filter.Bib (bibliography)
import Filter.Float (float)
import Filter.Hyperref (hyperref)
import Filter.LinksAsNotes (linksAsNotes)
import Filter.Macros (processMacros)
import Filter.MultiBib (multibib)
import Filter.Multicol (multicol)
import Filter.NumberRef (numberRef)
import Filter.WrapFloat (wrapFloat)
import Paths_ppp (version)
import PostProcess (trim)
import PreProcess (include)
import Reader (toPandoc)
import Writer (toTex, toPdf)

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Options.Applicative
import System.FilePath ((-<.>), takeExtension)


ppp :: FilePath -> IO T.Text
ppp fp
  =   fmap trim
  .   toTex
  =<< processMacros False
  .   hyperref
  .   float
  .   wrapFloat
  .   multibib
  =<< bibliography
  .   linksAsNotes
  .   numberRef
  .   multicol
  .   abstract
  .   simplifyAttr
  =<< processMacros True
  =<< toPandoc
  =<< include fp


data Options = Options
  { targetTex   :: Bool
  , sourceFiles :: [FilePath]
  }

parser :: Parser Options
parser
  =   infoOption ( "ppp version " ++ showVersion version )
        (  long "version"
        <> help "show version information and exit" )
  <*> abortOption ShowHelpText
        (  long "help"
        <> help "show usage information and exit" )
  <*> ( Options
        <$> switch
              (  long "tex"
              <> help "convert to latex instead of pdf" )
        <*> some ( strArgument ( metavar "<file>..." ) ) )

main :: IO ()
main = do
  opts <- execParser $ info parser idm
  forM_ (sourceFiles opts) $ \file -> case (targetTex opts, takeExtension file) of
    (False, ".md" ) -> do
                       putStrLn $ "rendering " ++ (file -<.> "pdf") ++ "..."
                       BS.writeFile (file -<.> "pdf") =<< toPdf =<< ppp file

    (False, ".tex") -> do
                       putStrLn $ "rendering " ++ (file -<.> "pdf") ++ "..."
                       BS.writeFile (file -<.> "pdf") =<< toPdf =<< T.readFile file

    (True,  ".md" ) -> do
                       putStrLn $ "rendering " ++ (file -<.> "tex") ++ "..."
                       T.writeFile  (file -<.> "tex") =<< ppp file

    (True,  ".tex") -> error $ file ++ ": is already in tex format"
    (_,     ext   ) -> error $ file ++ ": unrecognised file extension `" ++ ext ++ "'"
