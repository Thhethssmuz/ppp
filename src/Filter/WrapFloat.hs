module Filter.WrapFloat (wrapFloat) where

import BlockWriter

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Data.Char (isSpace)
import Data.Maybe
import Data.List (elemIndex, genericLength, intersperse, intercalate, zipWith4)
import Numeric (showFFloat)
import Text.Pandoc.Definition
import Text.Pandoc.Shared (splitBy, stringify)
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
getCaptionAttr (Para (Str s : is)) = case takeWhile' (/=':') s of
  "Figure:"  -> Just ("figure", caption, attr)
  "Table:"   -> Just ("table", caption, attr)
  "Formula:" -> Just ("formula", caption, attr)
  "Program:" -> Just ("program", caption, attr)
  "Example:" -> Just ("example", caption, attr)
  ":"        -> Just ("misc", caption, attr)
  _          -> Nothing
  where
    s'                  = drop 1 $ dropWhile (/=':') s
    (caption, attr)     = splitCaptionAttr . trimInline $ Str s' : is
    takeWhile' _ []     = []
    takeWhile' p (x:xs) = x : if p x then takeWhile' p xs else []
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

mkTable :: Block -> Attr -> BlockWriter
mkTable (Table caption al ws head rows) (i,cs,as) = do
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
  when (long && styleT) $ mkLongCaption "table" numb style i caption
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
  when (long && styleB) $ mkLongCaption "table" numb style i caption
  when (long && span) . tex $ "\\begin{pppmulticol}"



mkProgram :: Block -> [Inline] -> BlockWriter
mkProgram cb@(CodeBlock (i,cs,as) code) caption = do
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
    mkLongCaption "program" numb style i caption
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
    mkLongCaption "program" numb style i caption

  when (long && not span) . tex $ "\\end{ShadedLong}"
  when (long && span) . tex $ "\\begin{pppmulticol}"



boxTypes =
  [ "figure"
  , "table"
  , "formula"
  , "program"
  , "example"
  ]

pairWalk :: (Block -> Block -> (Block, Block)) -> [Block] -> [Block]
pairWalk _ []       = []
pairWalk f [x]      = case f x Null of
                        (x', Null) -> [x']
pairWalk f (x:y:bs) = case f x y of
                        (x', Null) -> pairWalk f (x' : bs)
                        (x', y'  ) -> x' : pairWalk f (y' : bs)

wrap :: String -> [Block] -> [Inline] -> Attr -> Block
wrap t bs [] (i,cs,as) = Div (i,"box":t:cs,as) bs
wrap t bs is (i,cs,as) = Div (i,"box":t:cs,as) (bs++[c])
  where c = Plain [Span ([],["caption"],[]) is]

pairTransform :: Block -> Block -> (Block, Block)
pairTransform (Para [Image attr caption (url, _)]) sibling =
  let env = "figure"
      fig = RawBlock (Format "tex")
          $ "\\includegraphics[width=\\linewidth]{" ++ url ++ "}"

  in  case fmap (merge (env, caption, attr)) $ getCaptionAttr sibling of
    Just (env, caption, attr) -> (wrap env [fig] caption attr, Null)
    Nothing                   -> (wrap env [fig] caption attr, sibling)

pairTransform (Table caption al ws bss bsss) sibling =
  let env    = "table"
      attr   = nullAttr

      render env caption attr@(_,cs,_) =
        let table = Table caption al ws bss bsss
            long  = elem "long" cs
            rndrd = Div nullAttr $ runBlockWriter (mkTable table attr) []
        in  if long then rndrd else wrap env [rndrd] caption attr

  in  case fmap (merge (env, caption, attr)) $ getCaptionAttr sibling of
    Just (env, caption, attr) -> (render env caption attr, Null)
    Nothing                   -> (render env caption attr, sibling)

pairTransform (CodeBlock attr code) sibling =
  let env     = "program"
      caption = []

      render env caption attr@(_,cs,_) =
        let prog  = CodeBlock attr code
            long  = elem "long" cs
            rndrd = Div nullAttr $ runBlockWriter (mkProgram prog caption) []
        in  if long then rndrd else wrap env [rndrd] caption attr

  in  case fmap (merge (env, caption, attr)) $ getCaptionAttr sibling of
    Just (env, caption, attr) -> (render env caption attr, Null)
    Nothing                   -> (render env caption attr, sibling)

pairTransform div@(Div attr@(_,cs,_) bs) sibling =
  let box = elem "box" cs

      env = fromMaybe "misc" . listToMaybe . filter (flip elem boxTypes) $ cs
      (bs', caption) = case splitBoxComponents bs of
        (bs', Just (Span _ is)) -> (bs', is)
        (bs', Nothing)          -> (bs', [])

  in  case fmap (merge (env, caption, attr)) $ getCaptionAttr sibling of
    Just (env, caption, attr) -> (wrap env bs' caption attr, Null)
    Nothing                   -> (div, sibling)

pairTransform block sibling = case getCaptionAttr sibling of
  Just (env, caption, attr) -> (wrap env [block] caption attr, Null)
  Nothing                   -> (block, sibling)



filterNull :: [Block] -> [Block]
filterNull = filter ((/=) Null)

wrapFloat :: Pandoc -> Pandoc
wrapFloat = walk (pairWalk pairTransform) . walk filterNull
