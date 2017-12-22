module Filter.WrapFloat (wrapFloat) where

import BlockWriter

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Data.Char (isSpace)
import Data.Maybe
import qualified Data.Map.Lazy as M
import Data.List (elemIndex, genericLength, intersperse, intercalate, zipWith4)
import Numeric (showFFloat, showIntAtBase)
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc.Definition
import Text.Pandoc.Shared (inlineListToIdentifier, splitBy, stringify)
import Text.Pandoc.Walk


showD :: Double -> String
showD d = showFFloat (Just 4) d ""

parseWidth :: String -> Maybe Double
parseWidth x = case reads x of
  [(d, "")]  -> Just d
  [(d, "%")] -> Just $ d / 100
  _          -> Nothing

parseInt :: String -> Maybe Int
parseInt x = case reads x of
  [(i, "")]  -> Just i
  _          -> Nothing


type BuildAttr = StateT Attr (Except ()) ()

buildAttr :: [Inline] -> Either () Attr
buildAttr xs = runExcept . execStateT (build xs) $ nullAttr

setId :: String -> BuildAttr
setId    i'  = modify (\(i,cs,as) -> (i',cs,as))

addClass :: String -> BuildAttr
addClass c   = modify (\(i,cs,as) -> (i,c:cs,as))

addAttr :: String -> String -> BuildAttr
addAttr  k v = modify (\(i,cs,as) -> (i,cs,(k,v):as))

build :: [Inline] -> BuildAttr
build is = case is of
  [] -> return ()
  (Space : xs) -> build xs
  (Str "" : xs) -> build xs
  (Str ('#':id) : xs) -> do
    let h = takeWhile (flip notElem "#.") id
        t = dropWhile (flip notElem "#.") id
    setId h
    build $ [Str t] ++ xs
  (Str ('.':c) : xs) -> do
    let h = takeWhile (flip notElem "#.") c
        t = dropWhile (flip notElem "#.") c
    addClass h
    build $ [Str t] ++ xs
  (Str k : Quoted _ v : xs) ->
    if not (null k) && last k == '='
      then addAttr (init k) (stringify v) >> build xs
      else lift $ throwE ()
  (Str x : xs) -> case splitBy ((==) '=') x of
    [k]   -> addAttr k "" >> build xs
    [k,v] -> addAttr k v >> build xs
    _     -> lift $ throwE ()
  _ -> lift $ throwE ()



trimInline :: [Inline] -> [Inline]
trimInline = f . reverse . f . reverse
  where f (Str "" : is) = f is
        f (Space : is)  = f is
        f is            = is

splitCaptionAttr :: [Inline] -> ([Inline], Attr)
splitCaptionAttr is =
  let (s,xs,ys) = foldr f (0, [], []) is

      f x@(Str str) (0, xs, ys) =
        if not (null str) && last str == '}'
          then f (Str $ init str) (1, xs, ys)
          else (0, x : xs, ys)
      f x (0, xs, ys) = (0, x : xs, ys)
      f x@(Str str) (1, xs, ys) =
        if elem '{' str
          then let h   = reverse . drop 1 . dropWhile (/= '{') . reverse $ str
                   t   = reverse . takeWhile (/= '{') . reverse $ str
                   xs' = if null h then xs else Str h : xs
                   ys' = if null t then ys else Str t : ys
               in  (2, xs', ys')
          else (1, xs, x : ys)
      f x (1, xs, ys) = (1, xs, x : ys)
      f x (2, xs, ys) = (2, x : xs, ys)

  in  if s /= 2 then (is, nullAttr) else case buildAttr ys of
        Right attr -> (trimInline xs, attr)
        Left  _    -> (is, nullAttr)

getCaptionAttr :: Block -> Maybe (String, [Inline], Attr)
getCaptionAttr (Para (Str s : is)) =
  let s'                  = takeWhile' (/=':') s
      isCaption           = not (null s') && last s' == ':'
      env                 = map (\x -> if x == '\160' then ' ' else x) $ init s'
      takeWhile' _ []     = []
      takeWhile' p (x:xs) = x : if p x then takeWhile' p xs else []
      is'                 = Str (drop 1 $ dropWhile (/=':') s) : is
      (caption, attr)     = splitCaptionAttr . trimInline $ is
  in  if isCaption then Just (env, caption, attr) else Nothing
getCaptionAttr _ = Nothing

merge :: (String, [Inline], Attr) -> (String, [Inline], Attr) -> (String, [Inline], Attr)
merge (xenv, xcaption, (xi,xcs,xas)) (yenv, ycaption, (yi,ycs,yas)) =
  let env     = if null yenv then xenv else yenv
      caption = if null ycaption then xcaption else ycaption
      i       = if null yi then xi else yi
      cs      = ycs ++ xcs
      as      = yas ++ xas
  in  (env, caption, (i,cs,as))

splitBoxComponents :: [Block] -> ([Block],Maybe Inline)
splitBoxComponents = foldr f ([], Nothing)
  where
    f b (bs,c) = if caption b then (bs, Just $ span b) else (b:bs, c)
    caption (Plain [Span (_,cs,_) _]) = elem "caption" cs
    caption (Para  [Span (_,cs,_) _]) = elem "caption" cs
    caption _ = False
    span (Plain [x]) = x
    span (Para  [x]) = x


mkLongCaption :: String -> Bool -> String -> String -> [Inline] -> BlockWriter
mkLongCaption env numb style i [] = return ()
mkLongCaption env numb style i caption = do
  when (style /= "plain") $ do
    tex $ "\\floatstyle{" ++ style ++ "}"
    tex $ "\\restylefloat{" ++ env ++ "}"

  tex $ "\\vspace{-\\intextsep}"
  tex $ "\\begin{" ++ env ++ (if numb then "" else "*") ++ "}[H]"

  tex $ "\\caption" ++ (if numb then "" else "*") ++ "{"
  inlines caption
  tex "}"

  unless (null i) $ do
    tex $ "\\label{" ++ i ++ "}"

  tex $ "\\end{" ++ env ++ (if numb then "" else "*") ++ "}"
  tex $ "\\vspace{-\\intextsep}"

  when (style /= "plain") $ do
    tex $ "\\floatstyle{plain}"
    tex $ "\\restylefloat{" ++ env ++ "}"


tableColumnAlignX =
  [ (AlignLeft, ("l", "\\raggedright"))
  , (AlignCenter, ("c", "\\centering"))
  , (AlignRight, ("r", "\\raggedleft"))
  , (AlignDefault, ("l", "\\raggedright"))
  ]

normalize :: [Double] -> [Double]
normalize xs
  | all (>0) xs = map (/ (sum xs)) xs
  | otherwise   = take (length xs) $ repeat (1.0 / (genericLength xs))

mkColumn :: Alignment -> Double -> TableCell -> Int -> BlockWriter
mkColumn a w bs s = do
  let a' = snd . fromJust . lookup a $ tableColumnAlignX
      w' = showD w

  when (w > 0 && not (isInfinite w)) $ do
    tex $ "\\begin{minipage}[b]{" ++ w' ++ "\\columnwidth-" ++ show s ++ "\\tabcolsep}"
    tex $ a' ++ "\\strut"

  blocks bs

  when (w > 0 && not (isInfinite w)) $ do
    tex $ "\\strut\\end{minipage}"

mkRow :: [Alignment] -> [Double] -> [TableCell] -> BlockWriter
mkRow al ws columns = do
  let sub1 []     = []
      sub1 (x:xs) = (x-1) : xs
      fl = sub1 . reverse . sub1 $ map (const 2) columns
  sequence_ . intersperse (tex "&") $ zipWith4 mkColumn al ws columns fl
  tex "\\tabularnewline"

mkTable :: String -> Block -> Attr -> BlockWriter
mkTable env (Table caption al ws head rows) (i,cs,as) = do
  let span    = elem "span" cs
      long    = elem "long" cs

      style   = fromMaybe "plain" $ lookup "style" as
      styleT  = elem style ["plaintop", "ruled"]
      styleB  = not styleT
      numb    = not (null caption) && "unnumbered" `notElem` cs

      norm    = elem "normalize" cs || elem "normalise" cs
      tstyle  = fromMaybe "plain" $ lookup "table-style" as
      align   = concatMap (fst . fromJust . flip lookup tableColumnAlignX) al
      widths  = (if norm then normalize else id)
              . fromMaybe ws $ do
                  xs <- lookup "column-widths" as
                  case catMaybes . map parseWidth . splitBy isSpace $ xs of
                    [x] -> return . take (length ws) $ repeat x
                    xs  -> do
                           guard $ length xs == length ws
                           return xs

  when (long && span) . tex $ "\\end{pppmulticol}"
  when long . tex $ "\\vspace{\\intextsep}"
  when (long && styleT) $ mkLongCaption env numb style i caption
  tex $ "\\begin{longtable*}{@{}" ++ align ++ "@{}}"

  unless (tstyle == "none") . tex $ "\\toprule"

  mkRow al widths head

  unless (tstyle == "none") . tex $ "\\midrule"
  tex $ "\\endfirsthead"
  unless (tstyle == "none") .tex $ "\\toprule"

  mkRow al widths head

  unless (tstyle == "none") . tex $ "\\midrule"
  tex $ "\\endhead"

  mapM_ (mkRow al widths) rows

  unless (tstyle == "none") . tex $ "\\bottomrule"

  tex $ "\\end{longtable*}"
  when (long && styleB) $ mkLongCaption env numb style i caption
  when (long && span) . tex $ "\\begin{pppmulticol}"


mkProgram :: String -> Block -> [Inline] -> BlockWriter
mkProgram env cb@(CodeBlock (i,cs,as) code) caption = do
  let span    = elem "span" cs
      long    = elem "long" cs
      style   = fromMaybe "plain" $ lookup "style" as
      styleT  = elem style ["plaintop", "ruled"]
      styleB  = not styleT
      numb    = not (null caption) && "unnumbered" `notElem` cs

  when (long && span) . tex $ "\\end{pppmulticol}"
  when long . tex $ "\\vspace{\\intextsep}"
  when (long && not span) . tex $ "\\begin{ShadedLong}"

  when (long && not (null caption) && styleT) $ do
    mkLongCaption env numb style i caption
    -- tex $ "\\vspace{-\\intextsep}"

  when long $ do
    tex $ "\\vspace{-\\parskip}"
    tex $ "\\begin{ppp-long-verbatim}"

  tex $ "%--trim--%"
  block cb
  tex $ "%--trim--%"

  when long $ do
    tex $ "\\end{ppp-long-verbatim}"
    -- tex $ "\\vspace{-\\topskip}"

  when (long && not (null caption) && styleB) $ do
    mkLongCaption env numb style i caption

  when (long && not span) . tex $ "\\end{ShadedLong}"
  when (long && span) . tex $ "\\begin{pppmulticol}"


type BoxS = State (M.Map String String)

mkId :: String -> BoxS String
mkId ""       = return "misc"
mkId "Figure" = return "figure"
mkId "Table"  = return "table"
mkId p = do
  existing <- gets $ M.lookup p
  case existing of
    Just i  -> return i
    Nothing -> do
      l <- gets M.size
      let i = "ppp-gen-float-" ++ showIntAtBase 26 (toEnum . (+97)) l ""
      modify $ M.insert p i
      return i


pairWalkM :: Monad m => (Block -> Block -> m (Block, Block)) -> [Block] -> m [Block]
pairWalkM _ []       = return []
pairWalkM f [x]      = do
                       pair <- f x Null
                       case pair of
                        (x', Null) -> return [x']
pairWalkM f (x:y:bs) = do
                       pair <- f x y
                       case pair of
                        (x', Null) -> fmap (x':) $ pairWalkM f bs
                        (x', y'  ) -> fmap (x':) $ pairWalkM f (y' : bs)

wrap :: String -> [Block] -> [Inline] -> Attr -> Block
wrap env bs [] (i,cs,as) = Div (i,"box":cs,("box-group", env):as) bs
wrap env bs is (i,cs,as) = Div (i,"box":cs,("box-group", env):as) (bs++[c])
  where c = Plain [Span ([],["caption"],[]) is]

pairTransform :: Block -> Block -> BoxS (Block, Block)
pairTransform (Para [Image attr caption (url, _)]) sibling = do
  let env = ""
      fig = RawBlock (Format "tex")
          $ "\\includegraphics[width=\\linewidth]{" ++ url ++ "}"

  case merge (env, caption, attr) <$> getCaptionAttr sibling of
    Just (env, caption, attr) -> do
                                 i <- mkId env
                                 return (wrap i [fig] caption attr, Null)
    Nothing                   -> do
                                 i <- mkId env
                                 return (wrap i [fig] caption attr, sibling)

pairTransform (Table caption al ws bss bsss) sibling = do
  let env    = ""
      attr   = nullAttr

      render env caption attr@(_,cs,_) =
        let table = Table caption al ws bss bsss
            long  = elem "long" cs
            rndrd = Div nullAttr $ runBlockWriter (mkTable env table attr) []
        in  if long then rndrd else wrap env [rndrd] caption attr

  case merge (env, caption, attr) <$> getCaptionAttr sibling of
    Just (env, caption, attr) -> do
                                 i <- mkId env
                                 return (render i caption attr, Null)
    Nothing                   -> do
                                 i <- mkId env
                                 return (render i caption attr, sibling)

pairTransform (CodeBlock attr code) sibling = do
  let env     = ""
      caption = []

      render env caption attr@(_,cs,_) =
        let prog  = CodeBlock attr code
            long  = elem "long" cs
            rndrd = Div nullAttr $ runBlockWriter (mkProgram env prog caption) []
        in  if long then rndrd else wrap env [rndrd] caption attr

  case merge (env, caption, attr) <$> getCaptionAttr sibling of
    Just (env, caption, attr) -> do
                                 i <- mkId env
                                 return (render i caption attr, Null)
    Nothing                   -> do
                                 i <- mkId env
                                 return (render i caption attr, sibling)

pairTransform div@(Div attr@(_,cs,as) bs) sibling = do
  let box = elem "box" cs

      env = fromMaybe "" . lookup "box-group" $ as
      (bs', caption) = case splitBoxComponents bs of
        (bs', Just (Span _ is)) -> (bs', is)
        (bs', Nothing)          -> (bs', [])

  case guard box >> merge (env, caption, attr) <$> getCaptionAttr sibling of
    Just (env, caption, attr) -> do
                                 i <- mkId env
                                 return (wrap i bs' caption attr, Null)
    Nothing                   -> return (div, sibling)

pairTransform block sibling = return (block, sibling)



filterNull :: [Block] -> [Block]
filterNull = filter ((/=) Null)

wrapFloat :: Pandoc -> Pandoc
wrapFloat doc =
  let (doc', groups) = flip runState M.empty
                     . walkM (pairWalkM pairTransform)
                     . walk filterNull $ doc
  in  ( setMeta "boxGroup" `flip` doc' )
      . MetaList
      . map (MetaMap . M.fromList)
      . map (\(k,v) -> [("id", MetaString v),("name", MetaString k)])
      . M.toList
      $ groups
