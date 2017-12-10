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
-- import Text.Pandoc.Walk
import Debug.Trace


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
      numb    = not (null caption) && "unnumbered" `notElem` cs
      env     = if long && numb then "longtable" else "longtable*"

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
  when (long) . tex $ "\\vspace{\\intextsep}"
  tex $ "\\begin{" ++ env ++ "}{@{}" ++ align ++ "@{}}"

  when (long && not (null caption) && style == "plaintop") $ do
    tex $ "\\caption" ++ (if numb then "" else "*") ++ "{"
    inlines caption
    tex $ "}\\\\"
  when (long && not (null i) && style == "plaintop") $ do
    tex $ "\\label{" ++ i ++ "}"

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

  when (long && not (null caption) && style /= "plaintop") $ do
    tex $ "\\caption" ++ (if numb then "" else "*") ++ "{"
    inlines caption
    tex $ "}"
  when (long && not (null i) && style /= "plaintop") $ do
    tex $ "\\label{" ++ i ++ "}"

  tex $ "\\end{" ++ env ++ "}"
  when (long && span) . tex $ "\\begin{pppmulticol}"



mkProgram :: Block -> [Inline] -> BlockWriter
mkProgram cb@(CodeBlock (i,cs,as) code) caption = do
  let span    = elem "span" cs
      long    = elem "long" cs
      style   = fromMaybe "plain" $ lookup "style" as
      numb    = not (null caption) && "unnumbered" `notElem` cs

  when (long && span) . tex $ "\\end{pppmulticol}"
  when (long && not span) . tex $ "\\begin{ShadedLong}"

  when (long && not (null caption) && style /= "plaintop") $ do
    tex "\\captionsetup{type=program}"

  when (long && not (null caption) && style == "plaintop") $ do
    tex "\\captionsetup{type=program}"
    tex $ "\\caption{"
    inlines caption
    tex $ "}"
  when (long && not (null i) && style == "plaintop") $ do
    tex $ "\\label{" ++ i ++ "}"

  block cb

  when (long && not (null caption) && style /= "plaintop") $ do
    tex $ "\\caption{"
    inlines caption
    tex $ "}"
  when (long && not (null i) && style /= "plaintop") $ do
    tex $ "\\label{" ++ i ++ "}"

  when (long && not span) . tex $ "\\end{ShadedLong}"
  when (long && span) . tex $ "\\begin{pppmulticol}"



pairWalk :: (Block -> Block -> (Either Block Block, Block)) -> Pandoc -> Pandoc
pairWalk f (Pandoc meta bs) = Pandoc meta $ pairWalk' bs
  where
    pairWalk' []       = []
    pairWalk' (x:y:bs) = case f x y of
                           -- If Right then recursively walk children
                           (Left  x', Null) -> x' : pairWalk' bs
                           (Left  x', y'  ) -> x' : pairWalk' (y' : bs)
                           (Right x', Null) -> (sub x') : pairWalk' bs
                           (Right x', y'  ) -> (sub x') : pairWalk' (y' : bs)
    pairWalk' (x:bs)   = case f x Null of
                           (Left  x', Null) -> x' : pairWalk' bs
                           (Left  x', y'  ) -> x' : pairWalk' (y' : bs)
                           (Right x', Null) -> (sub x') : pairWalk' bs
                           (Right x', y'  ) -> (sub x') : pairWalk' (y' : bs)

    sub (BlockQuote bs)        = BlockQuote $ pairWalk' bs
    sub (OrderedList a bss)    = OrderedList a $ map pairWalk' bss
    sub (BulletList bss)       = BulletList $ map pairWalk' bss
    sub (DefinitionList m)     = DefinitionList
                               $ map (\(is,bss) -> (is, map pairWalk' bss)) m
    sub (Table a b c bss bsss) = Table a b c (map pairWalk' bss)
                               $ map (map pairWalk') bsss
    sub (Div a bs)             = Div a $ pairWalk' bs
    sub b                      = b

wrap :: String -> Block -> [Inline] -> Attr -> Block
wrap t b [] (i,cs,as) = Div (i,"box":t:cs,as) [b]
wrap t b is (i,cs,as) = Div (i,"box":t:cs,as) [b, c]
  where c = Plain [Span ([],["caption"],[]) is]

pairTransform :: Block -> Block -> (Either Block Block, Block)
pairTransform (Para [Image attr caption (url, _)]) sibling =
  let fig = RawBlock (Format "tex")
          $ "\\includegraphics[width=\\linewidth]{" ++ url ++ "}"
  in  (Left $ wrap "figure" fig caption attr, sibling)

pairTransform (Table is al ws bss bsss) sibling =
  let (caption, attr@(_,cs,_)) = splitCaptionAttr $ trimInline is
      long  = elem "long" cs
      table = Table caption al ws bss bsss
      rndrd = Div nullAttr $ runBlockWriter (mkTable table attr) []
      final = if long then rndrd else wrap "table" rndrd caption attr
  in  (Right final, sibling)

pairTransform cb@(CodeBlock (pi,pcs,pas) code) sibling = case sibling of
  Para (Str ('P':'r':'o':'g':'r':'a':'m':':':s) : is) ->
    let (caption, (ci,ccs,cas)) = splitCaptionAttr . trimInline $ Str s : is
        attr@(i,cs,as) = (if null ci then pi else ci, ccs ++ pcs, cas ++ pas)
        cb'   = CodeBlock attr code
        long  = elem "long" cs
        rndrd = Div nullAttr $ runBlockWriter (mkProgram cb' caption) []
        final = if long then rndrd else wrap "program" rndrd caption attr
    in  (Left final, Null)
  Para (Str (':':s) : is) ->
    let (caption, (ci,ccs,cas)) = splitCaptionAttr . trimInline $ Str s : is
        attr@(i,cs,as) = (if null ci then pi else ci, ccs ++ pcs, cas ++ pas)
        cb'   = CodeBlock attr code
        long  = elem "long" cs
        rndrd = Div nullAttr $ runBlockWriter (mkProgram cb' caption) []
        final = if long then rndrd else wrap "program" rndrd caption attr
    in  (Left final, Null)
  _ ->
    (Left $ wrap "program" cb [] (pi,pcs,pas), sibling)

pairTransform form@(Para [math@(Math DisplayMath _)]) sibling =
  let ((caption, attr), sibling') = case sibling of
        Para (Str ('F':'o':'r':'m':'u':'l':'a':':':s) : is) ->
          (splitCaptionAttr . trimInline $ Str s : is, Null)
        Para (Str (':':s) : is) ->
          (splitCaptionAttr . trimInline $ Str s : is, Null)
        _ -> (([], nullAttr), sibling)
      space = "\\vspace{-\\abovedisplayskip}\\vspace{-.4\\normalbaselineskip}"
      form' = Plain [RawInline (Format "tex") space, math]
  in  (Left $ wrap "formula" form' caption attr, sibling')

pairTransform block sibling = (Right block, sibling)



wrapFloat :: Pandoc -> Pandoc
wrapFloat = pairWalk pairTransform
