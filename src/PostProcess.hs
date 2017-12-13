{-# LANGUAGE OverloadedStrings #-}

module PostProcess (trim) where

import qualified Data.Text as T

f :: [T.Text] -> [T.Text] -> [T.Text]
f ys [] = ys
f ys (x:xs)
  | token     = f (dropWhile T.null ys) (dropWhile T.null xs)
  | comment   = f ys xs
  | otherwise = f (x:ys) xs
  where
    token   = x == "%--trim--%"
    comment = not (T.null x) && T.head x == '%'

trim = T.intercalate "\n" . f [] . reverse . T.lines
