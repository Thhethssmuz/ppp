module Filter.Macros (processMacros) where

import Reader (toPandoc)

import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import qualified Data.Map.Lazy as M
import Data.Char (toLower, isSpace)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.List (intercalate)
import Text.Pandoc.Builder
import Text.Pandoc.Definition
import Text.Pandoc.Shared (addMetaField, splitBy)
import Text.Pandoc.Walk (walkM)
import System.IO (hPutStrLn, stderr)


parseInt :: String -> Maybe Int
parseInt x = case reads x of
  [(x,"")] -> Just x
  _        -> Nothing

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing


type MetaS = StateT Meta IO

class ToBlock a where
  toBlock :: a -> Block
instance ToBlock () where
  toBlock _ = Null
instance ToBlock Block where
  toBlock x = x

setGlobal :: ToMetaValue a => String -> a -> MetaS ()
setGlobal key val = do
  prev <- gets $ lookupMeta key
  case prev of
    Just _  -> warn $ "duplicate instances of global macro `" ++ key ++ "'"
    Nothing -> modify $ setMeta key val

addGlobal :: ToMetaValue a => String -> a -> MetaS ()
addGlobal key = modify . addMetaField key . toMetaValue

void :: MetaS () -> MetaS Block
void f = f >> return Null

warn = liftIO . hPutStrLn stderr


data Macro = Macro
  { macroName  :: String
  , macroLines :: [String]
  , macroSep   :: Int
  } deriving Show


failWith :: Macro -> String -> MetaS a
failWith m str = warn (macroName m ++ ": " ++ str) >> fail ""

parseInlines :: String -> MetaS [Inline]
parseInlines s = do
  (Pandoc _ [LineBlock [is]]) <- liftIO . toPandoc . ("| "++) $ s
  return is

parseBlocks :: String -> MetaS [Block]
parseBlocks s = liftIO (toPandoc s) >>= \(Pandoc _ bs) -> return bs


with :: ToBlock b
         => Macro
         -> (Macro -> Int -> MetaS ())
         -> (Macro -> MetaS [a])
         -> ([a] -> MetaS b)
         -> MetaS Block
with m q p f = do
  xs <- p m
  q m $ length xs
  fmap toBlock $ f xs <|> case xs of
    [x] -> failWith m $ "invalid argument"
    _   -> failWith m $ "one or more invalid arguments"


raw :: Macro -> MetaS [String]
raw m = case macroSep m of
  0 -> when (length (macroLines m) > 0) (failWith m "missing macro separator") >> return []
  1 -> return . map trim . filter (not . all isSpace) . macroLines $ m
  2 -> failWith m "does not support block arguments, try using ':' instead."
  _ -> failWith m "invalid macro separator"

raw' :: Macro -> MetaS [String]
raw' m = case macroSep m of
  0 -> when (length (macroLines m) > 0) (failWith m "missing macro separator") >> return []
  1 -> return . map trim . filter (not . all isSpace)
     . concatMap (splitBy isSpace) . macroLines $ m
  2 -> failWith m "does not support block arguments, try using ':' instead."
  _ -> failWith m "invalid macro separator"

inl :: Macro -> MetaS [[Inline]]
inl m = case macroSep m of
  0 -> when (length (macroLines m) > 0) (failWith m "missing macro separator") >> return []
  1 -> (mapM parseInlines . filter (not . all isSpace) . macroLines $ m)
   <|> failWith m "failed to parse argument(s)"
  2 -> failWith m "does not support block arguments, try using ':' instead."
  _ -> failWith m "invalid macro separator"

blk :: Macro -> MetaS [Block]
blk m = case macroSep m of
  0 -> when (length (macroLines m) > 0) (failWith m "missing macro separator") >> return []
  1 -> (:[]) . LineBlock <$> inl m
  2 -> parseBlocks . intercalate "\n" $ macroLines m
  _ -> failWith m "invalid macro separator"

blk' :: Macro -> MetaS [Block]
blk' m = case macroSep m of
  0 -> when (length (macroLines m) > 0) (failWith m "missing macro separator") >> return []
  1 -> map Plain <$> inl m <|> failWith m "failed to parse argument(s)"
  2 -> (parseBlocks . intercalate "\n" $ macroLines m)
   <|> failWith m "failed to parse argument(s)"
  _ -> failWith m "invalid macro separator"


none m 0 = return ()
none m i = failWith m $ "to many arguments, expected 0 got " ++ show i
one m 0 = failWith m $ "missing argument, expected 1 got 0"
one m 1 = return ()
one m i = failWith m $ "to many arguments, expected 1 got " ++ show i
atleastone m 0 = failWith m "missing argument, expected at least one got 0"
atleastone m i = return ()
whatever _ _ = return ()
oneof is m i = if elem i is then return () else do
  let e = intercalate ", " (map show $ init is) ++ " or " ++ show (last is)
  failWith m $ "invalid number of argument, expected " ++ e ++ " got " ++ show i


tex :: String -> Block
tex = RawBlock (Format "tex")
tex' :: String -> Inline
tex' = RawInline (Format "tex")
multiLine :: [[Inline]] -> Many Inline
multiLine = fromList . intercalate [LineBreak]


macro :: Bool -> Block -> Macro -> MetaS Block
macro pre block m = case map toLower $ macroName m of

  "type"            -> with m one raw $ \[r] -> case map toLower r of
                         "article" -> do
                                      setGlobal "article" True
                                      setGlobal "documentclass" $ MetaString "scrartcl"
                         "report"  -> do
                                      setGlobal "report" True
                                      setGlobal "documentclass" $ MetaString "scrreprt"
                         "book"    -> do
                                      setGlobal "book" True
                                      setGlobal "documentclass" $ MetaString "scrbook"
                         _         -> fail ""

  "pagesides"       -> with m one raw $ \[r] -> case r of
                         "1" -> setGlobal "page-twoside" False
                         "2" -> setGlobal "page-twoside" True
                         _   -> fail ""
  "pagesize"        -> with m one raw $ \[r] -> setGlobal "page-size" $ MetaString r
  "pageorientation" -> with m one raw $ \[r] -> case map toLower r of
                         "landscape" -> setGlobal "page-paper" $ MetaString "landscape"
                         "portrait"  -> setGlobal "page-paper" $ MetaString "portrait"
                         _           -> fail ""
  "pagecolumns"     -> with m (oneof [1,2,3]) raw' $ \rs -> do
                         let [c] = filter (isJust . parseInt) $ rs
                             bs  = filter ( flip elem ["balanced", "unbalanced"]
                                          . map toLower
                                          ) rs
                             as  = filter (`notElem` (c:bs)) rs
                         guard $ length bs <= 1
                         guard $ length as <= 1
                         return . tex $
                           "\\end{pppmulticol}\n" ++
                           "\\def\\pppcoldepth{" ++ c ++ "}\n" ++
                           ( case bs of
                               ["balanced"]   -> "  \\pppbalancemulticoltrue\n"
                               ["unbalanced"] -> "  \\pppbalancemulticolfalse\n"
                               _              -> "" ) ++
                           ( case as of
                               [a] -> "  \\setlength\\columnsep{" ++ a ++ "}\n"
                               _   -> "" ) ++
                           "\\begin{pppmulticol}"
  "pagediv"         -> with m one raw $ setGlobal "page-div" . MetaString . head
  "pagebcor"        -> with m one raw $ setGlobal "page-bcor" . MetaString . head

  "fontsize"        -> with m one raw $ setGlobal "font-size" . MetaString . head
  "fontmain"        -> with m one raw $ setGlobal "main-font" . MetaString . head
  "fontsans"        -> with m one raw $ setGlobal "sans-font" . MetaString . head
  "fontmono"        -> with m one raw $ setGlobal "mono-font" . MetaString . head
  "fontmath"        -> with m one raw $ setGlobal "math-font" . MetaString . head

  "header"          -> let f xs y    = [tex' $ "\\" ++ y ++ "head[]{"] ++ xs ++ [tex' "}"]
                           g xs ys b = return . Plain . mc . (++) (sep b) . concat . zipWith f xs $ ys
                           sep b     = [tex' $ "\\setheadsepline" ++ if b then "[text]{.4pt}" else "[0mm]{0mm}"]
                           h1 xs     = g xs ["o","c","i"]
                           h2 xs     = g xs ["le","ce","re","lo","co","ro"]
                           mc xs     = [tex' "\\end{pppmulticol}"] ++ xs ++
                                       [tex' "\\begin{pppmulticol}"]
                           n         = []
                           dmy x     = if x == [Str "-"] then n else x
                       in  with m (oneof [0,1,2,3,4,6]) inl $ \ls -> case map dmy ls of
                         [le,ce,re,lo,co,ro] -> h2 [le,ce,re,lo,co,ro] True
                         [le,re,lo,ro]       -> h2 [le,n,re,lo,n,ro] True
                         [o,c,i]             -> h1 [o,c,i] True
                         [o,i]               -> h1 [o,n,i] True
                         [c]                 -> h1 [n,c,n] True
                         []                  -> h1 [n,n,n] False
  "footer"          -> let f xs y  = [tex' $ "\\" ++ y ++ "foot["] ++ xs ++ [tex' "]{"] ++ xs ++ [tex' "}"]
                           g xs ys = return . Plain . mc . concat . zipWith f xs $ ys
                           h1 xs   = g xs ["o","c","i"]
                           h2 xs   = g xs ["le","ce","re","lo","co","ro"]
                           mc xs   = [tex' "\\end{pppmulticol}"] ++ xs ++
                                     [tex' "\\begin{pppmulticol}"]
                           n       = []
                           dmy x   = if x == [Str "-"] then n else x
                       in  with m (oneof [0,1,2,3,4,6]) inl $ \ls -> case map dmy ls of
                         [le,ce,re,lo,co,ro] -> h2 [le,ce,re,lo,co,ro]
                         [le,re,lo,ro]       -> h2 [le,n,re,lo,n,ro]
                         [o,c,i]             -> h1 [o,c,i]
                         [o,i]               -> h1 [o,n,i]
                         [c]                 -> h1 [n,c,n]
                         []                  -> h1 [n,n,n]

  "titlepage"       -> with m whatever blk $ \bs -> do
                         setGlobal "titlepage" $ MetaBool True
                         if length bs > 0
                           then return $ Div nullAttr bs
                           else return . tex $ "\\end{pppmulticol}\n" ++
                                               "\\pppmaketitle\n" ++
                                               "\\begin{pppmulticol}"
  "titlehead"       -> with m (oneof [0,1,2,3]) blk' $ \bs -> do
                         let dmy x    = if x == Para [Str "-"] then Null else x
                             box as xs = Div ("",["box"],as) xs
                         case map dmy bs of
                           [l,c,r] -> setGlobal "title-head" . MetaBlocks . (:[]) $
                                        box [] [ box [("align", "top left")] [l]
                                               , box [("align", "top centre")] [c]
                                               , box [("align", "top right")] [r]
                                               ]
                           [l,r]   -> setGlobal "title-head" . MetaBlocks . (:[]) $
                                        box [] [ box [("align", "top left")] [l]
                                               , box [("align", "top right")] [r]
                                               ]
                           [c]     -> setGlobal "title-head" . MetaBlocks . (:[]) $
                                        box [] [ box [("align", "top centre")] [c] ]
                           []      -> return ()

  "subject"         -> with m one blk $ setGlobal "subject" . MetaBlocks
  "title"           -> with m one blk $ setGlobal "title" . MetaBlocks
  "subtitle"        -> with m one blk $ setGlobal "subtitle" . MetaBlocks
  "author"          -> with m atleastone inl $ \ls -> addGlobal "author" . M.fromList $
                         [ ( "head", MetaInlines $ head ls )
                         , ( "full", MetaInlines $ intercalate [LineBreak] ls )
                         ]
  "authors"         -> with m atleastone inl $ \ls -> mapM_ (addGlobal "author")
                         . map (\x -> M.fromList [("head", x), ("full", x)])
                         . map MetaInlines $ ls
  "date"            -> with m one blk $ setGlobal "date" . MetaBlocks
  "publisher"       -> with m one blk $ setGlobal "publisher" . MetaBlocks
  "keywords"        -> with m atleastone inl $ mapM_ (addGlobal "keywords" . MetaInlines)

  "tableofcontents" -> return . tex $ "\\tableofcontents{}"
  "listof"          -> with m one inl $ \[l] -> case safeHead l of
                         Just (Str "Figure") -> return . tex $ "\\listof{figure}{}"
                         Just (Str "Table")  -> return . tex $ "\\listof{table}{}"
                         Just (Str r)        -> if pre then return block else do
                           meta <- get
                           let r' = map (\x -> if x == '\160' then ' ' else x) r
                               unlist (MetaList xs)  = Just xs
                               unlist _              = Nothing
                               unstr  (MetaString s) = Just s
                               unstr  _              = Nothing
                               unmap  (MetaMap xs)   = do
                                                       i <- unstr =<< M.lookup "id" xs
                                                       n <- unstr =<< M.lookup "name" xs
                                                       return (n,i)
                               unmap  _              = Nothing
                               group = do
                                 gs <- unlist =<< lookupMeta "boxGroup" meta
                                 g  <- lookup r' . catMaybes . map unmap $ gs
                                 return . tex $ "\\listof{" ++ g ++ "}{}"
                           maybe (fail "") return group
                         _ -> fail ""

  "csl"             -> with m one raw $ setGlobal "csl" . MetaString . head
  "csllocale"       -> with m one raw $ setGlobal "csllocale" . MetaString . head
  "bibliography"    -> with m whatever raw $ \rs -> do
                       mapM_ (addGlobal "bibliography" . MetaString) rs
                       return $ Div ("refs",["references"],[]) [Plain [Span ("",["refmarker"],[]) []]]
  "notes"           -> with m none raw $ \_ -> do
                         modify $ setMeta "pppnotes" True
                         return . tex $ "\\pppnotes"

  "chapterprefix"   -> with m whatever inl $ \ls -> do
                         let b = if length ls > 0 then "true" else "false"
                         return . Plain $
                           [ tex' $ "\\end{pppmulticol}\n"] ++
                           [ tex' $ "\\pppchapterprefix{" ++ b ++ "}{" ] ++
                           intercalate [LineBreak] ls ++
                           [ tex' $ "}\n" ] ++
                           [ tex' $ "\\begin{pppmulticol}" ]
  "tocdepth"        -> with m (oneof [0,1]) raw $ \rs -> case rs of
                         []  -> return . tex $ "\\tocdepth{0}"
                         [r] -> return . tex $ "\\tocdepth{" ++ r ++ "}"
  "numdepth"        -> with m (oneof [0,1]) raw $ \rs -> case rs of
                         []  -> return . tex $ "\\numdepth{0}"
                         [r] -> return . tex $ "\\numdepth{" ++ r ++ "}"
  "numstyle"        -> with m one raw $ \[r] -> do
                         let f r = "\\end{pppmulticol}\n" ++
                                   "\\numstyle{\\" ++ r ++ "}{" ++ r ++ "}\n" ++
                                   "\\begin{pppmulticol}"
                         return . tex . f $ case map toLower r of
                           "numeric"      -> "arabic"
                           "number"       -> "arabic"
                           "num"          -> "arabic"
                           "alphabetical" -> "Alph"
                           "alphabetic"   -> "Alph"
                           "alpha"        -> "Alph"
                           "roman"        -> "Roman"
                           "rom"          -> "Roman"
                           _              -> fail ""
  "bmkdepth"        -> with m (oneof [0,1]) raw $ \rs -> case rs of
                         []  -> return . tex $ "\\bmkdepth{0}"
                         [r] -> return . tex $ "\\bmkdepth{" ++ r ++ "}"
  "bmkreset"        -> with m none raw $ \_ -> return . tex $ "\\bmkreset{}"

  "frontmatter"     -> with m whatever raw $ \rs -> do
                         a <- macro pre Null $ Macro "NumDepth" ["1"] 1
                         b <- macro pre Null $ Macro "TocDepth" ["1"] 1
                         c <- macro pre Null $ Macro "NumStyle" ["Roman"] 1
                         d <- macro pre Null $ m { macroName = "ChapterPrefix" }
                         return $ Div nullAttr [a,b,c,d]
  "mainmatter"      -> with m whatever raw $ \rs -> do
                         a <- macro pre Null $ Macro "NumDepth" ["3"] 1
                         b <- macro pre Null $ Macro "TocDepth" ["3"] 1
                         c <- macro pre Null $ Macro "NumStyle" ["Numeric"] 1
                         d <- macro pre Null $ m { macroName = "ChapterPrefix" }
                         return $ Div nullAttr [a,b,c,d]
  "appendices"      -> with m whatever raw $ \rs -> do
                         a <- macro pre Null $ Macro "NumDepth" ["3"] 1
                         b <- macro pre Null $ Macro "TocDepth" ["3"] 1
                         c <- macro pre Null $ Macro "NumStyle" ["Alphabetical"] 1
                         d <- macro pre Null $ m { macroName = "ChapterPrefix" }
                         return $ Div nullAttr [a,b,c,d]
  "backmatter"      -> with m whatever raw $ \rs -> do
                         a <- macro pre Null $ Macro "NumDepth" [] 1
                         b <- macro pre Null $ Macro "TocDepth" ["3"] 1
                         c <- macro pre Null $ Macro "NumStyle" ["Numeric"] 1
                         d <- macro pre Null $ m { macroName = "ChapterPrefix" }
                         e <- macro pre Null $ Macro "BmkReset" [] 1
                         return $ Div nullAttr [a,b,c,d,e]

  "part"            -> with m atleastone inl $ \ls -> return . Plain $
                         [ tex' "\\end{pppmulticol}\n\\part*{" ] ++
                         intercalate [LineBreak] ls ++
                         [ tex' "}\n\\addcontentsline{toc}{part}{" ] ++
                         head ls ++
                         [ tex' "}\n\\begin{pppmulticol}" ]

  "linksasnotes"    -> with m none raw $ \_ -> setGlobal "links-as-notes" True
  "includehead"     -> with m whatever blk $ addGlobal "includehead" . MetaBlocks

  _                 -> if pre
                         then return block
                         else void . warn $ "unknown macro `" ++ macroName m ++ "'"



processMacro :: Bool -> Block -> MetaS Block
processMacro pre block@(Div (i,cs,as) bs) =
  case guard (elem "ppp-macro" cs) >> lookup "macro" as of
    Nothing      -> return block
    Just ('%':_) -> return Null
    Just key     -> do
      let s  = fromMaybe 0 $ parseInt =<< lookup "sep" as
          l  = fromMaybe 0 $ parseInt =<< lookup "lines" as
          rs = catMaybes $ map (\i -> lookup ('l' : show i) as) [0..l]
          m  = Macro key rs s
      macro pre block m <|> return Null
processMacro _ block = return block

processMacros :: Bool -> Pandoc -> IO Pandoc
processMacros b (Pandoc meta bs) = do
  (bs', meta') <- runStateT (walkM (processMacro b) bs) meta
  return $ Pandoc meta' bs'
