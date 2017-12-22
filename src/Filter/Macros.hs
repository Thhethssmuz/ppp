module Filter.Macros (processMacros) where

import Control.Applicative ((<|>))
import Control.Monad (guard, zipWithM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import qualified Data.Map.Lazy as M
import Data.Char (toLower, isSpace)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.List (intercalate)
import Text.Pandoc.Builder
import Text.Pandoc.Definition
import Text.Pandoc.Shared (addMetaField, splitBy)
import Text.Pandoc.Walk (walkM, query)
import System.IO (hPutStrLn, stderr)


parseInt :: String -> Maybe Int
parseInt x = case reads x of
  [(x,"")] -> Just x
  _        -> Nothing

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

with :: ToBlock a => [Int] -> String -> [String] -> ([String] -> MetaS a) -> MetaS Block
with [-1] macro rs f = case length rs >= 1 of
  False -> void . warn $ macro ++ ": missing argument, expected at least one got 0"
  True  -> fmap toBlock (f rs) <|> case rs of
    [r] -> void . warn $ macro ++ ": invalid argument `" ++ r ++ "'"
    _   -> void . warn $ macro ++ ": one or more invalid arguments"
with [i] macro rs f = case compare (length rs) i of
  LT -> void . warn $ macro ++ ": missing argument, expected " ++ show i ++
                               " got " ++ show (length rs)
  EQ -> fmap toBlock (f rs) <|> case rs of
    [r] -> void . warn $ macro ++ ": invalid argument `" ++ r ++ "'"
    _   -> void . warn $ macro ++ ": one or more invalid arguments"
  GT -> void . warn $ macro ++ ": to many arguments, expected " ++ show i ++
                               " got " ++ show (length rs)
with is macro rs f = case length rs `elem` is of
  True  -> fmap toBlock (f rs) <|> case rs of
    [r] -> void . warn $ macro ++ ": invalid argument `" ++ r ++ "'"
    _   -> void . warn $ macro ++ ": one or more invalid arguments"
  False -> void . warn $ macro ++ ": invalid number of argument, expected " ++
                                  intercalate ", " (init rs) ++ " or " ++
                                  last rs ++ " got " ++ show (length rs)

withMany :: ToBlock a => String -> [String] -> ([String] -> MetaS a) -> MetaS Block
withMany macro rs f = with [-1] macro rs f
with1 :: ToBlock a => String -> [String] -> (String -> MetaS a) -> MetaS Block
with1 macro rs f = with [1] macro rs $ f . head
with0 :: ToBlock a => String -> [String] -> MetaS a -> MetaS Block
with0 macro rs f = with [0] macro rs $ \_ -> f


tex :: String -> Block
tex = RawBlock (Format "tex")
tex' :: String -> Inline
tex' = RawInline (Format "tex")
multiLine :: [[Inline]] -> Many Inline
multiLine = fromList . intercalate [LineBreak]


macro :: Bool -> Block -> String -> [String] -> [[Inline]] -> MetaS Block
macro pre block m rs ls = case map toLower m of

  "type"            -> with1 m rs $ \r -> case map toLower r of
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

  "pagesides"       -> with1 m rs $ \r -> case r of
                         "1" -> setGlobal "page-twoside" False
                         "2" -> setGlobal "page-twoside" True
                         _   -> fail ""
  "pagesize"        -> with1 m rs $ setGlobal "page-size" . MetaString
  "pageorientation" -> with1 m rs $ \r -> case map toLower r of
                         "landscape" -> setGlobal "page-paper" $ MetaString "landscape"
                         "portrait"  -> setGlobal "page-paper" $ MetaString "portrait"
                         _           -> fail ""
  "pagecolumns"     -> with [1,2,3] m (concatMap (splitBy isSpace) rs) $ \rs' -> do
                         let [c] = filter (isJust . parseInt) $ rs'
                             bs  = filter (`elem` ["balanced", "unbalanced"]) rs
                             as  = filter (`notElem` (c:bs)) rs'
                         guard $ length bs <= 1
                         guard $ length as <= 1
                         return . tex $
                           "\\ifnum\\pppcoldepth=" ++ c ++ "\n" ++
                           "\\else\n" ++
                           "  \\end{pppmulticol}\n" ++
                           "  \\def\\pppcoldepth{" ++ c ++ "}\n" ++
                           ( case bs of
                               ["balanced"]   -> "  \\pppbalancemulticoltrue\n"
                               ["unbalanced"] -> "  \\pppbalancemulticolfalse\n"
                               _              -> "" ) ++
                           ( case as of
                               [a] -> "  \\setlength\\columnsep{" ++ a ++ "}\n"
                               _   -> "" ) ++
                           "  \\begin{pppmulticol}\n" ++
                           "\\fi"
  "pagediv"         -> with1 m rs $ setGlobal "page-div" . MetaString
  "pagebcor"        -> with1 m rs $ setGlobal "page-bcor" . MetaString

  "fontsize"        -> with1 m rs $ setGlobal "font-size" . MetaString
  "fontmain"        -> with1 m rs $ setGlobal "main-font" . MetaString
  "fontsans"        -> with1 m rs $ setGlobal "sans-font" . MetaString
  "fontmono"        -> with1 m rs $ setGlobal "mono-font" . MetaString
  "fontmath"        -> with1 m rs $ setGlobal "math-font" . MetaString

  "header"          -> let f xs y    = [tex' $ "\\" ++ y ++ "head[]{"] ++ xs ++ [tex' "}"]
                           g xs ys b = return . Plain . (++) (sep b) . concat . zipWith f xs $ ys
                           sep b     = [tex' $ "\\setheadsepline" ++ if b then "[text]{.4pt}" else "[0mm]{0mm}"]
                           h1 xs     = g xs ["o","c","i"]
                           h2 xs     = g xs ["le","ce","re","lo","co","ro"]
                           n         = []
                           dmy x     = if x == [Str "-"] then n else x
                       in  with [0,1,2,3,4,6] m rs $ \_ -> case map dmy ls of
                         [le,ce,re,lo,co,ro] -> h2 [le,ce,re,lo,co,ro] True
                         [le,re,lo,ro]       -> h2 [le,n,re,lo,n,ro] True
                         [o,c,i]             -> h1 [o,c,i] True
                         [o,i]               -> h1 [o,n,i] True
                         [c]                 -> h1 [n,c,n] True
                         []                  -> h1 [n,n,n] False
  "footer"          -> let f xs y  = [tex' $ "\\" ++ y ++ "foot["] ++ xs ++ [tex' "]{"] ++ xs ++ [tex' "}"]
                           g xs ys = return . Plain . concat . zipWith f xs $ ys
                           h1 xs   = g xs ["o","c","i"]
                           h2 xs   = g xs ["le","ce","re","lo","co","ro"]
                           n       = []
                           dmy x     = if x == [Str "-"] then n else x
                       in  with [0,1,2,3,4,6] m rs $ \_ -> case map dmy ls of
                         [le,ce,re,lo,co,ro] -> h2 [le,ce,re,lo,co,ro]
                         [le,re,lo,ro]       -> h2 [le,n,re,lo,n,ro]
                         [o,c,i]             -> h1 [o,c,i]
                         [o,i]               -> h1 [o,n,i]
                         [c]                 -> h1 [n,c,n]
                         []                  -> h1 [n,n,n]

  "titlepage"       -> do
                       setGlobal "titlepage" $ MetaBool True
                       if length ls > 0
                         then return . Plain . intercalate [LineBreak] $ ls
                         else return . tex $ "\\end{pppmulticol}\n" ++
                                             "\\pppmaketitle\n" ++
                                             "\\begin{pppmulticol"
  "titlehead"       -> let [hl,hc,hr]    = map ("title-head-"++) ["left","centre","right"]
                           f x [Str "-"] = return ()
                           f x y         = setGlobal x $ MetaInlines y
                       in  with [0,1,2,3] m rs $ \_ -> case ls of
                         [l,c,r] -> zipWithM_ f [hl,hc,hr] [l,c,r]
                         [l,r]   -> zipWithM_ f [hl,hr] [l,r]
                         [c]     -> zipWithM_ f [hc] [c]
                         []      -> return ()
  "subject"         -> void . setGlobal "subject" . multiLine $ ls
  "title"           -> void . setGlobal "title" . multiLine $ ls
  "subtitle"        -> void . setGlobal "subtitle" . multiLine $ ls
  "author"          -> withMany m rs $ \_ -> addGlobal "author" . M.fromList $
                         [ ( "head", MetaInlines $ head ls )
                         , ( "full", MetaInlines $ intercalate [LineBreak] ls )
                         ]
  "authors"         -> withMany m rs $ \_ -> mapM_ (addGlobal "author")
                         . map (\x -> M.fromList [("head", x), ("full", x)])
                         . map MetaInlines $ ls
  "date"            -> void . setGlobal "date" . multiLine $ ls
  "publisher"       -> void . setGlobal "publisher" . multiLine $ ls
  "keywords"        -> void . mapM_ (addGlobal "keywords" . fromList) $ ls

  "tableofcontents" -> return . tex $ "\\tableofcontents{}"
  "listof"          -> with1 m rs $ \_ -> case safeHead =<< safeHead ls of
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

  "csl"             -> with1 m rs $ setGlobal "csl" . MetaString
  "csllocale"       -> with1 m rs $ setGlobal "csllocale" . MetaString
  "bibliography"    -> do
                       mapM_ (addGlobal "bibliography" . MetaString) rs
                       return $ Div ("refs",["references"],[]) [Plain [Span ("",["refmarker"],[]) []]]
  "notes"           -> with0 m rs $ do
                         modify $ setMeta "pppnotes" True
                         return . tex $ "\\pppnotes"

  "chapterprefix"   -> let b = if length rs > 0 then "true" else "false"
                       in  return . Plain $
                         [ tex' $ "\\pppchapterprefix{" ++ b ++ "}{" ] ++
                         intercalate [LineBreak] ls ++
                         [ tex' $ "}" ]
  "tocdepth"        -> with1 m rs $ \r -> return . tex $ "\\tocdepth{" ++ r ++ "}"
  "numdepth"        -> with1 m rs $ \r -> return . tex $ "\\numdepth{" ++ r ++ "}"
  "numstyle"        -> let f r = "\\end{pppmulticol}\n" ++
                                 "\\numstyle{\\" ++ r ++ "}{" ++ r ++ "}\n" ++
                                 "\\begin{pppmulticol}"
                       in  with1 m rs $ \r -> return . tex . f $ case map toLower r of
                           "numeric"      -> "arabic"
                           "number"       -> "arabic"
                           "num"          -> "arabic"
                           "alphabetical" -> "Alph"
                           "alphabetic"   -> "Alph"
                           "alpha"        -> "Alph"
                           "roman"        -> "Roman"
                           "rom"          -> "Roman"
                           _              -> fail ""
  "bmkdepth"        -> with1 m rs $ \r -> return . tex $ "\\bmkdepth{" ++ r ++ "}"
  "bmkreset"        -> with0 m rs . return . tex $ "\\bmkreset{}"

  "frontmatter"     -> with0 m rs $ do
                         a <- macro pre Null "numdepth" [] []
                         b <- macro pre Null "tocdepth" [] []
                         c <- macro pre Null "numstyle" ["numeric"] []
                         return $ Div nullAttr [a,b,c]
  "mainmatter"      -> with0 m rs $ do
                         a <- macro pre Null "numdepth" ["3"] []
                         b <- macro pre Null "tocdepth" ["3"] []
                         c <- macro pre Null "numstyle" ["numeric"] []
                         return $ Div nullAttr [a,b,c]
  "appendices"      -> with0 m rs $ do
                         a <- macro pre Null "numdepth" ["3"] []
                         b <- macro pre Null "tocdepth" ["3"] []
                         c <- macro pre Null "numstyle" ["alphabetical"] []
                         return $ Div nullAttr [a,b,c]
  "backmatter"      -> with0 m rs $ do
                         a <- macro pre Null "numdepth" [] []
                         b <- macro pre Null "tocdepth" ["3"] []
                         c <- macro pre Null "numstyle" ["numeric"] []
                         d <- macro pre Null "bmkreset" [] []
                         return $ Div nullAttr [a,b,c,d]

  "part"            -> withMany m rs $ \rs -> return . Plain $
                         [ tex' "\\end{pppmulticol}\n\\part*{" ] ++
                         intercalate [LineBreak] ls ++
                         [ tex' "}\n\\addcontentsline{toc}{part}{" ] ++
                         head ls ++
                         [ tex' "}\n\\begin{pppmulticol}" ]

  "linksasnotes"    -> with0 m rs $ setGlobal "links-as-notes" True
  "includehead"     -> void . addGlobal "includehead" . rawInline "tex"
                            . intercalate "\n" $ rs

  _                 -> if pre
                         then return block
                         else void . warn $ "unknown macro `" ++ m ++ "'"



processMacro :: Bool -> Block -> MetaS Block
processMacro pre block@(Div (i,cs,as) bs) = do
  let lines (Span (_,cs,_) is) = if elem "line" cs then [is] else []
      lines _ = []
  case guard (elem "ppp-macro" cs) >> lookup "macro" as of
    Just ('%':_) -> return Null
    Just key     -> let l  = fromMaybe 0 $ parseInt =<< lookup "lines" as
                        rs = catMaybes $ map (\i -> lookup ('l' : show i) as) [0..l]
                        ls = query lines bs
                    in  macro pre block key rs ls
    Nothing      -> return block
processMacro _ block = return block

processMacros :: Bool -> Pandoc -> IO Pandoc
processMacros b (Pandoc meta bs) = do
  (bs', meta') <- runStateT (walkM (processMacro b) bs) meta
  return $ Pandoc meta' bs'
