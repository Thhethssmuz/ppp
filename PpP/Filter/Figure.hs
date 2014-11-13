module PpP.Filter.Figure (figure) where

import Text.ParserCombinators.Parsec
import Control.Applicative

import Text.Pandoc
import Text.Pandoc.Walk

import Data.List.Split (splitOn)
import Numeric         (showFFloat)
import Data.List       (genericLength)
import Data.Maybe      (maybeToList, isJust)
import Data.Char       (toLower)

type Scale   = Float
data Pos     = L | C | F | R deriving (Show, Eq)
type Loc     = (Scale, Pos)
type Ref     = String
type Caption = [Inline]

data Figure  = Figure [Caption] [FilePath] Pos Scale Ref
             deriving (Show)

parseScale :: Parser Scale
parseScale = do
  p <- many1 digit
  n <- option "" $ (:) <$> char '.' <*> many1 digit
  char '%'
  return . read $ p ++ n

parseLoc :: Bool -> Parser Loc
parseLoc b = do
  l <- option ' ' $ char '<'
  p <- parseScale
  r <- option ' ' $ char '>'

  case [l, r] of "< " -> return (p, L)
                 "<>" -> return (p, if b then F else C)
                 "  " -> return (p, C)
                 " >" -> return (p, R)

processTarget :: Bool -> Target -> (Scale, Pos, [FilePath], Ref)
processTarget b (fp, r) =
  let  fps = splitOn "%20" fp in
  case parse (parseLoc b) "" (last fps) of
    Left    _   -> (1.0, C, fps, r)
    Right (s,p) -> (s / 100.0, p, init fps, r)


processCaptions :: [Inline] -> [Caption]
processCaptions xs = foldr f [[]] xs where
  f (Str x) []     = [[Str x]]
  f (Str x) (a:as) = if elem ';' x
                     then let s = splitOn ";" x in
                          map (return . Str) (init s) ++ (Str (last s):a):as
                     else (Str x:a):as
  f x       (a:as) = (x:a):as


fromImage :: Bool -> [Inline] -> Target -> Figure
fromImage b is t = Figure cs fs p s r
  where (s, p, fs, r) = processTarget b t
        cs            = processCaptions is


tex :: String -> Inline
tex = RawInline (Format "tex")

showF f = showFFloat (Just 4) f ""

toInline :: Figure -> [Inline]
toInline (Figure _  []     _ _ _) = error "missing image url"

toInline (Figure cs (f:[]) F s r) =
  let a = do
          c  <- take 1 cs
          c' <- take 1 c
          [ tex $ "\\caption{" ] ++ c ++ [
            tex $ "}\n",
            tex $ "\\label{" ++ r ++ "}\n"
            ]
  in [
    tex $ "\\begin{figure*}[tbp]\n",
    tex $ "\\centering\n",
    tex $ "\\includegraphics[width=" ++ showF s ++ "\\textwidth]{" ++ f ++ "}\n" ]
    ++ a ++ [
    tex $ "\\end{figure*}"
    ]

toInline (Figure cs (f:[]) C s r) =
  let a = do
          c  <- take 1 cs
          c' <- take 1 c
          [ tex $ "\\caption{" ] ++ c ++ [
            tex $ "}\n",
            tex $ "\\label{" ++ r ++ "}\n"
            ]
  in [
    tex $ "\\begin{figure}[H]\n",
    tex $ "\\centering\n",
    tex $ "\\includegraphics[width=" ++ showF s ++ "\\textwidth]{" ++ f ++ "}\n" ]
    ++ a ++ [
    tex $ "\\vspace{-11pt}\n",
    tex $ "\\end{figure}"
    ]

toInline (Figure cs (f:[]) p s r) =
  let s' = showF s ++ "\\textwidth"
      a  = do
           c  <- take 1 cs
           c' <- take 1 c
           [ tex $ "\\caption{" ] ++ c ++ [
             tex $ "}\n",
             tex $ "\\label{" ++ r ++ "}\n"
             ]
  in [
    tex $ "\\begin{wrapfigure}{" ++ (map toLower (show p)) ++ "}{" ++ s' ++ "}\n",
    tex $ "\\centering\n",
    tex $ "\\includegraphics[width=" ++ s' ++ "]{" ++ f ++ "}\n" ]
    ++ a ++ [
    tex $ "\\end{wrapfigure}"
    ]

toInline (Figure cs fs     p s r) =
  let s' = showF $ s / genericLength fs - 0.01 in [
  tex $ "\\begin{figure" ++ (if p == F then "*}[tbp]\n" else "}[H]\n"),
  tex $ "\\centering\n"] ++ do

  (f, i) <- zip fs [1..]

  let a = do
          c  <- take 1 . drop i $ cs
          c' <- take 1 c
          [ tex $ "\\caption{" ] ++ c ++ [
            tex $ "}\n",
            tex $ "\\label{" ++ (r ++ ['-', toEnum $ 96 + i]) ++ "}\n"
            ]

  [ tex $ "\\begin{subfigure}[t]{" ++ s' ++ "\\textwidth}\n",
    tex $ "\\includegraphics[width=\\textwidth]{" ++ f ++ "}\n" ] ++ a ++ [
    tex $ "\\end{subfigure}\n"
    ]

  ++ do 
     c  <- take 1 cs
     c' <- take 1 c
     [ tex $ "\\caption{" ] ++ c ++ [
       tex $ "}\n",
       tex $ "\\label{" ++ r ++ "}\n"
       ]

  ++ [ tex $ "\\end{figure" ++ (if p == F then "*" else "") ++  "}" ]

wrap :: [Inline] -> Inline
wrap is = Span ("", [], []) $ [pre] ++ is ++ [post]
  where pre  = tex "}"
        post = tex "{"

processFigures :: Bool -> Inline -> Inline
processFigures b (Image is to) = wrap . toInline $ fromImage b is to
processFigures _ x = x

figure :: Pandoc -> Pandoc
figure pandoc@(Pandoc meta _) =
  let twocolumn = isJust . lookupMeta "page-twocolumn" $ meta
  in  walk (processFigures twocolumn) pandoc
