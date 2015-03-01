{-# LANGUAGE FlexibleContexts #-}

module PpP.Filter.Float (float) where

import PpP.Filter.Multicol

import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Generic (topDown)

import Text.ParserCombinators.Parsec

import Numeric (showFFloat)
import Data.Char (toUpper)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Control.Monad

-------------------------------------------------------------------------------

showF :: Float -> String
showF f = showFFloat (Just 4) f ""

parsePercent :: String -> Float
parsePercent = f . reads
  where f [(f', "")]  = f'
        f [(f', "%")] = f' / 100
        f _           = 1

parsePercent' :: String -> Maybe Float
parsePercent' = f . reads
  where f [(f', "")]  = Just f'
        f [(f', "%")] = Just $ f' / 100
        f _           = Nothing

tex = RawInline (Format "tex")

when' :: Bool -> (a -> a) -> a -> a
when' True f x = f x
when' _    _ x = x

-------------------------------------------------------------------------------

wrapSubfloatI :: String -> Bool -> [String] -> Inline -> Inline
wrapSubfloatI wh s [x,y] i = Span ([],[],[]) $
  let s'  = if s then "\\textwidth" else "\\linewidth" 
  in [ tex $ "\n\\begin{minipage}["++x++"]{"++wh++s'++"}\n",
       tex $ y++"{\n",
       i,
       tex $ "}\n",
       tex $ "\\end{minipage}\n" ]

wrapSubfloatB :: String -> Bool -> [String] -> Block -> [Block]
wrapSubfloatB wh s [x,y] b =
  let s' = if s then "\\textwidth" else "\\linewidth"
  in [ Plain $
       [ tex $ "\\begin{minipage}["++x++"]{"++wh++s'++"}",
         tex $ y ],
       b,
       Plain $ 
       [ tex $ "\\end{minipage}" ] ]

unSpan :: [Inline] -> [Inline]
unSpan ((Span ([],[],[]) x):xs) = x ++ unSpan xs
unSpan (x:xs) = [x] ++ unSpan xs
unSpan [] = []

concatPlain :: [Block] -> [Block]
concatPlain ((Plain x1):(Plain x2):xs) = concatPlain ((Plain $ x1 ++ x2):xs)
concatPlain (x:xs) = x : concatPlain xs
concatPlain [] = []

-------------------------------------------------------------------------------

mkCaptionI :: Bool -> Bool -> Inline -> Inline
mkCaptionI sub stared (Span (_,["caption"],_) is) = Span ([],[],[]) $
  let sub' = if sub then "sub" else ""
      st   = if stared then "*" else ""
  in [ tex $ "\\" ++ sub' ++ "caption" ++ st ++ "{" ] ++ is ++ [ tex "}" ]

mkCaptionB :: Bool -> Bool -> Block -> Block
mkCaptionB sub stared (Para [Span (_,["caption"],_) is]) = Plain $
  let sub' = if sub then "sub" else ""
      st   = if stared then "*" else ""
  in [ tex $ "\\" ++ sub' ++ "caption" ++ st ++ "{" ] ++ is ++ [ tex "}" ]
mkCaptionB sub stared (Plain [Span (_,["caption"],_) is]) = Plain $
  let sub' = if sub then "sub" else ""
      st   = if stared then "*" else ""
  in [ tex $ "\\" ++ sub' ++ "caption" ++ st ++ "{" ] ++ is ++ [ tex "}" ]

-------------------------------------------------------------------------------

mkSubfloatI :: String -> Inline -> Inline
mkSubfloatI t (Span (i,cs,as) is) = Span ([],[],[]) $ 
  let sub = filter (not . isCaptionI) . filter (/= Space) $ is
      lb  = mkLabel t i
      cps = unSpan . map (mkCaptionI True (t=="misc")) . filter isCaptionI $ is
  in sub ++ cps ++ lb

mkSubfloatB :: String -> Block -> Block
mkSubfloatB t (Div (i,cs,as) bs) = Div ([],[],[]) $
  let sub = filter (not . isCaptionB) bs
      lb  = mkLabel t i
      cps = map (mkCaptionB True (t=="misc")) . filter isCaptionB $ bs
  in sub ++ concatPlain (cps ++ [Plain lb])

-------------------------------------------------------------------------------

mkFloatI :: Int -> Inline -> Inline
mkFloatI m (Span (i,cs,as) is) = Span ([],[],[]) $
  let t   = typeCheck [ x | x <- types, x `elem` cs]

      f   = "float" `elem` cs
      s   = "span" `elem` cs
      w   = not s && "wrap" `elem` cs
      y   = isJust . lookup "style" $ as

      ft  = if f && (m > 1 || s) then "tbh" else "H"
      st  = if s && f then "*" else ""
      wt  = wrapTypeCheck f . lookup "wrap" $ as
      yt  = maybe "plain" styleCheck . lookup "style" $ as
      al  = alignCheck . lookup "align" $ as

      --is' = filter isFloatI $ is
      --nis = genericLength is'
      is' = filter (not . isCaptionI) . filter (/= Space) $ is
      nis = genericLength is'

      wh  = showF . when' (not w) ((*) (1 - 0.03 * (nis-1)) . (/nis))
          . maybe 1 parsePercent . lookup "width" $ as
      lb  = mkLabel t i

      cps = unSpan . map (mkCaptionI False (t=="misc")) . filter isCaptionI $ is
      sub = intersperse (tex "\\hspace{\\fill}")
          . map (wrapSubfloatI wh s al . subfloatI t) $ is'

  in [ tex $ "}\\end{pppmulticol}" | s && not f ] ++
     [ tex $  "\\floatstyle{"++yt++"}\\restylefloat{"++t++"}" | y ] ++
     [ tex "\n",
       tex $ if w 
         then "\\begin{wrapfloat}{"++t++"}{"++wt++"}{"++wh++"\\linewidth}\n"
         else "\\begin{"++t++st++"}["++ft++"]\n",
       tex $  "\\centering\n" ] ++
     sub ++
     cps ++
     lb ++
     [ tex $ if w
         then "\n\\end{wrapfloat}\n"
         else "\n\\end{"++t++st++"}\n" ] ++
     [ tex $  "\\floatstyle{plain}\\restylefloat{"++t++"}" | y ] ++
     [ tex $ "\\begin{pppmulticol}{" | s && not f ]

mkFloatB :: Int -> Block -> Block
mkFloatB m (Div (i, cs, as) bs) = Div ([],[],[]) $
  let t   = typeCheck [ x | x <- types, x `elem` cs]

      f   = "float" `elem` cs
      s   = "span" `elem` cs
      w   = not s && "wrap" `elem` cs
      y   = isJust . lookup "style" $ as

      ft  = if f && (m > 1 || s) then "tbh" else "H"
      st  = if s && f then "*" else ""
      wt  = wrapTypeCheck f . lookup "wrap" $ as
      yt  = maybe "plain" styleCheck . lookup "style" $ as
      al  = alignCheck . lookup "align" $ as

      bs' = filter (not . isCaptionB) bs
      nbs = genericLength bs'

      wh  = showF . when' (not w) ((*) (1 - 0.03 * (nbs-1)) . (/nbs))
          . maybe 1 parsePercent . lookup "width" $ as
      lb  = mkLabel t i

      cps = map (mkCaptionB False (t=="misc")) . filter isCaptionB $ bs
      sub = concat . intersperse [Plain [tex "\\hspace{\\fill}"]]
          . map (wrapSubfloatB wh s al . subfloatB t) $ bs'

  in concatPlain $ [ Plain $
       [ tex $ "\\end{pppmulticol}" | s && not f ] ++
       [ tex $  "\\floatstyle{"++yt++"}\\restylefloat{"++t++"}" | y ] ++
       [ tex $ if w 
           then "\\begin{wrapfloat}{"++t++"}{"++wt++"}{"++wh++"\\linewidth}"
           else "\\begin{"++t++st++"}["++ft++"]",
         tex $  "\\centering" ]
     ] ++ sub ++
     cps ++
     [ Plain $
       lb ++
       [ tex $ if w
           then "\\end{wrapfloat}"
           else "\\end{"++t++st++"}" ] ++
       [ tex $  "\\floatstyle{plain}\\restylefloat{"++t++"}" | y ] ++
       [ tex $ "\\begin{pppmulticol}" | s && not f ]
     ]

-------------------------------------------------------------------------------

wrapCaptionI :: [Inline] -> [Inline]
wrapCaptionI is = if null . filter (/= (Str "")) . filter (/= Space) $ is
                  then []
                  else [Span ([],["caption"],[]) is]

mkLabel :: String -> String -> [Inline]
mkLabel t i = if i == "" then [] else [ tex $ "\\label{" ++ prefix t ++ i ++ "}" ]

-------------------------------------------------------------------------------

isCaptionI :: Inline -> Bool
isCaptionI (Span (_,cs,_) _) = "caption" `elem` cs
isCaptionI _ = False

isCaptionB :: Block -> Bool
isCaptionB (Para [Span (_,cs,_) _]) = "caption" `elem` cs
isCaptionB (Plain [Span (_,cs,_) _]) = "caption" `elem` cs
isCaptionB _ = False

isFloatI :: Inline -> Bool
isFloatI (Span (_,cs,_) _) = "box" `elem` cs
isFloatI _ = False

isFloatB :: Block -> Bool
isFloatB (Div (_,cs,_) _) = "box" `elem` cs
isFloatB _ = False

isSubfloatI :: Inline -> Bool
isSubfloatI (Span (_,cs,_) _) = any (`elem` ["sub", "auto"]) cs
isSubfloatI _ = False

isSubfloatB :: Block -> Bool
isSubfloatB (Plain [Span (_,cs,_) _]) = any (`elem` ["sub", "auto"]) cs
isSubfloatB (Para [Span (_,cs,_) _]) = any (`elem` ["sub", "auto"]) cs
isSubfloatB (Div (_,cs,_) _) = any (`elem` ["sub", "auto"]) cs
isSubfloatB _ = False

-------------------------------------------------------------------------------

types = ["figure", "table", "formula", "program", "example", "misc"]

prefix "figure"  = "fig:"
prefix "table"   = "tab:"
prefix "formula" = "form:"
prefix "program" = "prog:"
prefix "example" = "ex:"
prefix "misc"    = ""
prefix _         = ""

typeCheck :: [String] -> String
typeCheck [x] = x
typeCheck _   = "misc"

wrapTypeCheck :: Bool -> Maybe String -> String
wrapTypeCheck float wrap = 
  when' float (map toUpper)
  . fromMaybe "o" $ do
      x <- wrap
      guard $ x `elem` ["outer", "inner", "left", "right"]
      return . take 1 $ x

styleCheck :: String -> String
styleCheck x =
  if x `elem` ["plain", "plaintop", "boxed", "ruled"] then x else "plain"

tableStyleCheck :: String -> String
tableStyleCheck x =
  if x `elem` ["plain", "ruled"] then x else "ruled"

alignCheck :: Maybe String -> [String]
alignCheck align =
  let as    = maybe ["top", "centre"] words align
      xs    = [ "top", "centre", "center", "bottom" ]
      ys    = [ "left", "centre", "center", "justified", "right" ]
      f "l" = "\\raggedright"
      f "c" = "\\centering"
      f "j" = ""
      f "r" = "\\raggedleft"
  in case as of
    [x,y]  -> [ if x `elem` xs then take 1 x else "t",
                f (if y `elem` ys then take 1 y else "c") ]
    _      -> [ "t", "c" ]

-------------------------------------------------------------------------------

subfloatI :: String -> Inline -> Inline
subfloatI t i = if isFloatI i && isSubfloatI i then mkSubfloatI t i else i

subfloatB :: String -> Block -> Block
subfloatB t b = if isFloatB b && isSubfloatB b then mkSubfloatB t b else b

floatI :: Int -> Inline -> Inline
floatI m i = if isFloatI i then mkFloatI m i else i

floatB :: Int -> Block -> Block
floatB m b = if isFloatB b then mkFloatB m b else b

-------------------------------------------------------------------------------

data AttrPart = IdPart String
              | ClassPart String
              | AttrPart (String, String)
              deriving (Show, Eq)

joinAttrParts :: [AttrPart] -> Attr
joinAttrParts parts = foldl f ([],[],[]) parts
  where f (_,cs,as) (IdPart x)    = (x,cs,as)
        f (i,cs,as) (ClassPart x) = (i,cs++[x],as)
        f (i,cs,as) (AttrPart x)  = (i,cs,as++[x])

parseIdPart :: Parser AttrPart
parseIdPart = do
  char '#'
  i <- many1 (alphaNum <|> oneOf "-_")
  spaces
  return $ IdPart i

parseClassPart :: Parser AttrPart
parseClassPart = do
  char '.'
  i <- many1 (alphaNum <|> oneOf "-_")
  spaces
  return $ ClassPart i

parseAttrPart :: Parser AttrPart
parseAttrPart = do
  k <- many1 alphaNum
  char '='
  char '"'
  v <- many $ noneOf "\""
  char '"'
  spaces
  return $ AttrPart (k,v)

parseAttr :: Parser Attr
parseAttr = do
  char '{'
  ps <- many $ parseIdPart <|> parseClassPart <|> parseAttrPart
  char '}'
  spaces
  eof
  return . joinAttrParts $ ps

parseUrl :: Parser FilePath
parseUrl = do
  x  <- noneOf "\\{" <|> (char '\\' >> anyChar)
  xs <- many $ (char '\\' >> anyChar) <|> noneOf " "
  spaces
  return (x:xs)

parseUrlAttr :: Parser ([FilePath], Attr)
parseUrlAttr = do
  fps  <- many1 parseUrl
  attr <- try parseAttr <|> return ([],[],[])
  return (fps, attr)

-------------------------------------------------------------------------------

stringify' :: Walkable Inline a => a -> String
stringify' = stringify . walk f
  where f :: Inline -> Inline
        f (Quoted DoubleQuote xs) = Span ([],[],[]) $ [Str "\""] ++ xs ++ [Str "\""]
        f x = x

extractAttrString :: [Inline] -> String
extractAttrString ((Str x):xs)
  | elem '{' x = stringify' $ (Str $ dropWhile (/='{') x):xs
  | otherwise  = extractAttrString xs
extractAttrString (_:xs) = extractAttrString xs
extractAttrString _ = ""

extractCaption :: [Inline] -> [Inline]
extractCaption ((Str x):xs)
  | elem '{' x = [Str $ takeWhile (/='{') x]
  | otherwise = (Str x) : extractCaption xs
extractCaption (x:xs) = x : extractCaption xs
extractCaption [] = []

splitCaptionAttr :: [Inline] -> ([Inline], Attr)
splitCaptionAttr is = 
  let as = extractAttrString is
      cp = extractCaption is
  in case parse parseAttr "" as of
  Left  _    -> (is, ([],[],[]))
  Right attr -> (cp, attr)

splitUrlAttr :: String -> ([FilePath], Attr)
splitUrlAttr s = case parse parseUrlAttr "" s of
  Left  err        -> ([s], ([],[],[]))
  Right (fps,attr) -> (fps,attr)

splitCaptions :: [Inline] -> [[Inline]]
splitCaptions xs = foldr f [[]] xs where
  f (Str x) (a:as)
    | ';' `elem` x = let s = splitOn ";" x
                     in  map (return . Str) (init s) ++ (Str (last s):a):as
    | otherwise    = (Str x:a):as
  f x       (a:as) = (x:a):as

fixCBAttr :: Attr -> Attr
fixCBAttr (i,cs,as) =
  let (i':is) = splitOn "." i
      cs'     = concatMap (splitOn ".") $ cs
  in  (i',cs'++is,as)

-------------------------------------------------------------------------------

transformI :: Inline -> Inline
transformI (Image _ (url, title)) =
  let (us, (i,cs,as)) = splitUrlAttr . unwords . splitOn "%20" $ url
      width = case lookup "width" as of
                Nothing -> ""
                Just x  -> case parsePercent' x of
                  Nothing -> "width=" ++ x
                  Just y  -> "width=" ++ showF y ++ "\\linewidth"
      inner = do
        u <- us
        return . tex $ "\\includegraphics[" ++ width ++ "]{" ++ u ++ "}"
  in Span ([],[],[]) inner

transformI i = i

-------------------------------------------------------------------------------

alignment :: Alignment -> (String, String)
alignment a = case a of
  AlignLeft    -> ("l", "\\raggedright")
  AlignRight   -> ("r", "\\raggedleft")
  AlignCenter  -> ("c", "\\centering")
  AlignDefault -> ("l", "\\raggedright")

wrapTableCell :: String -> String -> String -> String -> TableCell -> [Block]
wrapTableCell l r w a x =
  if w == "NaN"
  then x
  else [Plain [ tex $ "\\begin{minipage}["++r++"]{"++w++"\\columnwidth-"++l++"\\tabcolsep"++"}"++a++"\\strut\n" ]] ++
       x ++
       [Plain [ tex $ "\n\\strut\\end{minipage}" ]]

mkTableRow :: String -> [String] -> [String] -> [TableCell] -> [Block]
mkTableRow ra ws as xs =
  let g = genericLength ws
      l = if g == 1 then "1" else showF $ g / g / 2
  in  concatMap concatPlain
    . flip (++) ([[Plain [tex "\\tabularnewline\n"]]])
    . intersperse ([Plain [tex " \\and\n"]])
    . zipWith3 (wrapTableCell l ra) ws as $ xs

mkTable :: [Alignment]
        -> [Double]
        -> [TableCell]
        -> [[TableCell]]
        -> String
        -> [Block]
mkTable aligns widths heads rows ts =
  let (ls,as) = unzip . map alignment $ aligns
      ls'     = concat ls
      ws      = map (showF . realToFrac) widths

      b   = [ Plain [ tex $ "\\begin{tabular}{@{}" ++ ls' ++ "@{}}\n" ] ]
      tr  = [ Plain [ tex $ "\\toprule\n"    | ts == "ruled" ] ]
      x   = mkTableRow "b" ws as heads
      mr  = [ Plain [ tex $ "\\midrule\n"    | ts == "ruled" ] ]
      xm  = if all null heads then [] else x ++ mr
      xs  = map (mkTableRow "t" ws as) $ rows
      br  = [ Plain [ tex $ "\\bottomrule\n" | ts == "ruled" ] ]
      e   = [ Plain [ tex $ "\\end{tabular}" ] ]
  in  concatPlain (b ++ tr ++ xm ++ concat xs ++ br ++ e)

-------------------------------------------------------------------------------

transformB :: Block -> Block
transformB (Para [Image is (url, title)]) =
  let (us, (i,cs,as)) = splitUrlAttr . unwords . splitOn "%20" $ url
      (cp:cps) = splitCaptions is
      i'       = if title == "fig:" then i else drop 4 title
      inner    = do
        (u,c,l) <- zip3 us (map wrapCaptionI $ cps ++ repeat [])
                 . zipWith (++) (repeat $ i' ++ "-")
                 . map ((:"") . toEnum) $ [97..]
        return . Span (l,["sub","box"],[]) $ [
                   tex $ "\\includegraphics[width=\\linewidth]{"++u++"}"] ++ c

  in Para [Span (i',"auto":"figure":"box":cs,as) $ inner ++ wrapCaptionI cp]

transformB (Table is alig space heads cols) =
  let (cp, (i,cs,as)) = splitCaptionAttr is
      cp' = if any ((==) $ stringify cp) ["", " "] then [] else [Plain [Span ([],["caption"],[]) cp]]
      lb  = mkLabel "table" i
      l   = "long" `elem` cs
      w   = fmap (realToFrac . parsePercent) . lookup "width" $ as
      s   = maybe space (\w -> map ((*) . (/) w . sum $ space) $ space) w
      f   = maybe space (\w -> map ((*) . (/) 1 . sum $ space) $ space) w
      ts  = maybe "ruled" tableStyleCheck . lookup "tablestyle" $ as
  in case l of
    True -> Div ([],[],[]) [
              Plain [ tex "\\end{pppmulticol}" ],
              Table (cp++lb) alig s heads cols,
              Plain [ tex "\\begin{pppmulticol}" ] ]
    _    -> Div (i,"auto":"table":"box":cs,as) $ (
              mkTable alig f heads cols ts ) ++ cp'

transformB cb@(CodeBlock attr code) =
  let (i,cs,as) = fixCBAttr attr
      s   = "span" `elem` cs
      l   = "long" `elem` cs

      lb  = mkLabel "program" i
      cp1 = maybeToList $ do
              c <- lookup "caption" as
              return . tex $ "\\pppcaptionof{"++c++"}"
      cp2 = maybeToList 
          . fmap (\x -> Plain [Span ([],["caption"],[]) [Str x]])
          . lookup "caption" $ as
      
  in case (l,s) of 
        (True,True)  -> Div ([],[],[]) [
                          Plain $ [ tex "\\end{pppmulticol}" ] ++ cp1 ++ lb,
                          CodeBlock ([],cs,as) code,
                          Plain $ [ tex "\\begin{pppmulticol}" ] ]
        (True,False) -> Div ([],[],[]) [
                          Plain $ [ tex "\\begin{unShaded}" ] ++ cp1 ++ lb,
                          CodeBlock ([],cs,as) code,
                          Plain $ [ tex "\\end{unShaded}" ] ]
        _            -> Div (i,"auto":"program":"box":cs,as) $ [
                          CodeBlock ([],cs,as) code ] ++ cp2

transformB b = b

-------------------------------------------------------------------------------

float :: Pandoc -> Pandoc
float pandoc =
  let c = getPageColumns pandoc
  in      topDown (floatI c)
        . topDown (floatB c)
        . walk transformI
        . walk transformB
        $ pandoc
