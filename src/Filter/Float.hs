module Filter.Float (float) where

import BlockWriter

import Control.Applicative ((<|>))
import Control.Monad
import Data.Char (toUpper)
import Data.Maybe
import Data.List (genericLength, intersperse)
import Numeric (showFFloat)
import Text.Pandoc.Definition
import Text.Pandoc.Generic (topDown)


showF :: Float -> String
showF f = showFFloat (Just 4) f ""

parseWidth :: String -> Maybe Float
parseWidth x = case reads x of
  [(f, "")]  -> Just f
  [(f, "%")] -> Just $ f / 100
  _          -> Nothing

boxAlignX =
  [ ("left", "\\raggedright")
  , ("centre", "\\centering")
  , ("center", "\\centering")
  , ("justified", "")
  , ("right", "\\raggedleft")
  ]

boxAlignY =
  [ ("top", "t")
  , ("centre", "c")
  , ("center", "c")
  , ("bottom", "b")
  ]

boxAlignW =
  [ ("outer", "o")
  , ("left", "l")
  , ("right", "r")
  , ("inner", "i")
  ]

boxStyles = ["plain", "plaintop", "boxed", "ruled"]

splitBoxComponents :: [Block] -> ([Block],Maybe Inline)
splitBoxComponents = foldr f ([], Nothing)
  where
    f b (bs,c) = if caption b then (bs, Just $ span b) else (b:bs, c)
    caption (Plain [Span (_,cs,_) _]) = elem "caption" cs
    caption (Para  [Span (_,cs,_) _]) = elem "caption" cs
    caption _ = False
    span (Plain [x]) = x
    span (Para  [x]) = x

blockWidth :: Block -> Maybe Float
blockWidth (Div (_,_,as) bs) = lookup "width" as >>= parseWidth
blockWidth _ = Nothing

getEnv :: Block -> String
getEnv (Div (i,cs,as) bs) =
  fromMaybe "misc" . lookup "box-group" $ as
getEnv _ = "misc"


mkFloat :: Maybe Block -> Maybe Float -> Block -> BlockWriter
mkFloat parent inheritedWidth this@(Div (i,cs,as) bs) = do
  let box   = elem "box" cs
      float = elem "float" cs
      span  = elem "span" cs
      wrap  = isNothing parent && not span && "wrap" `elem` cs

      (children, caption) = if box then splitBoxComponents bs else (bs, Nothing)

      env   = getEnv this
      env'  = env ++ if float then "*" else ""
      penv  = fmap getEnv parent

      dw    = if wrap then 0.5 else 1
      width = fromMaybe dw $ inheritedWidth <|> blockWidth this
      width'= if wrap then 1 else width
      style = (\x -> if elem x boxStyles then x else "plain")
            . fromMaybe "plain" . lookup "style" $ as
      pos   = if isNothing parent && float && not wrap then "tbh" else "H"

      -- len   = if isNothing parent && span then "\\textwidth" else "\\linewidth"
      (x,y) = fromMaybe ("\\centering","t") $ do
                as <- fmap words . lookup "align" $ as
                guard $ length as == 2
                let xFirst = (,) <$> (lookup (as !! 0) boxAlignX)
                                 <*> (lookup (as !! 1) boxAlignY)
                    yFirst = (,) <$> (lookup (as !! 1) boxAlignX)
                                 <*> (lookup (as !! 0) boxAlignY)
                xFirst <|> yFirst
      w     = (if float then map toUpper else id)
            . fromMaybe "o" $ do
                a <- lookup "wrap" as
                lookup a boxAlignW

      numb  = isJust caption && env /= "misc" && "unnumbered" `notElem` cs
      cenv  = (if Just env == penv then "sub" else "") ++
              "caption" ++ (if numb then "" else "*")

  when (isNothing parent && span && not float) $ do
    tex $ "\\end{pppmulticol}"

  when (box && isJust parent) $ do
    -- tex $ "\\cprotect\\fbox{\\begin{minipage}[" ++ y ++ "]{" ++ showF width ++ "\\linewidth-2\\fboxsep-2\\fboxrule}%"
    tex $ "\\begin{minipage}[" ++ y ++ "]{" ++ showF width ++ "\\linewidth}"

  when (box && wrap) $ do
    tex $ "\\begin{pppwrapfloat}{misc}{" ++ w ++ "}{" ++ showF width ++ "\\linewidth}"
    -- tex $ "\\cprotect\\fbox{\\begin{minipage}[" ++ y ++ "]{\\linewidth-2\\fboxsep-2\\fboxrule}%"
    tex $ "\\begin{minipage}[" ++ y ++ "]{\\linewidth}"

  when (box && style /= "plain") $ do
    tex $ "\\floatstyle{" ++ style ++ "}"
    tex $ "\\restylefloat{" ++ env ++ "}"

  when (box && Just env /= penv) $ do
    tex $ "\\begin{" ++ env' ++ "}[" ++ pos ++ "]"

  when (box && not (null x)) $ do
    tex $ x

  when (isJust caption && style == "plaintop") $ do
    tex $ "\\" ++ cenv ++ "{"
    inline $ fromJust caption
    tex "}"
    unless (null i) $ do
      tex $ "\\label{" ++ i ++ "}"

  when (isNothing parent) $ do
    -- tex $ "\\cprotect\\fbox{\\begin{minipage}[" ++ y ++ "]{" ++ showF width' ++ "\\linewidth-2\\fboxsep-2\\fboxrule}%"
    tex $ "\\begin{minipage}[" ++ y ++ "]{" ++ showF width' ++ "\\linewidth}"
    tex $ x
    tex $ "\\begingroup"
    tex $ "\\setlength{\\intextsep}{0pt}"

  tex $ "%--trim--%"

  if not box
    then blocks bs
    else do
      let ws  = map blockWidth children
          s   = (-) 1 . sum . catMaybes $ ws
          l   = genericLength . filter isNothing $ ws
          w   = if l > 0 then s / l else 1
          ws' = (\ws -> if length children > 1
                          then map (fmap (subtract 0.015)) ws
                          else ws)
              . map (<|> Just w) $ ws

      sequence_ . intersperse (tex "%--trim--%\n\\hfill\n%--trim--%")
                . zipWith (mkFloat $ Just this) ws'
                $ children

  tex $ "%--trim--%"

  when (isNothing parent) $ do
    tex $ "\\endgroup"
    -- tex $ "\\end{minipage}}"
    tex $ "\\end{minipage}"

  when (isJust caption && style /= "plaintop") $ do
    tex $ "\\" ++ cenv ++ "{"
    inline $ fromJust caption
    tex "}"
    unless (null i) $ do
      tex $ "\\label{" ++ i ++ "}"

  when (isNothing parent && not wrap) $ do
    tex $ "\\vspace{-\\intextsep}"

  when (box && Just env /= penv) $ do
    tex $ "\\end{" ++ env' ++ "}"

  when (box && style /= "plain") $ do
    tex $ "\\floatstyle{plain}"
    tex $ "\\restylefloat{" ++ env ++ "}"

  when (box && wrap) $ do
    -- tex $ "\\end{minipage}}"
    tex $ "\\end{minipage}"
    tex $ "\\end{pppwrapfloat}"

  when (box && isJust parent) $ do
    -- tex $ "\\end{minipage}}"
    tex $ "\\end{minipage}"

  when (isNothing parent && span && not float) $ do
    tex $ "\\begin{pppmulticol}"

mkFloat parent inheritedWidth this =
  mkFloat parent inheritedWidth $ Div ([],[],[]) [this]


toplevel :: Block -> Block
toplevel this@(Div (_,cs,_) _) = if elem "box" cs then float else this
  where
    float = Div ("",[],[])
          $ runBlockWriter (mkFloat Nothing Nothing this) []
toplevel this = this

float :: Pandoc -> Pandoc
float = topDown toplevel
