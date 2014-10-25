module PpP.Filter.Err (printErrors) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk


getInline :: Inline -> [String]
getInline (Span (_,["citeproc-not-found"],[("data-reference-id",ref)]) _) =
  ["pandoc-citeproc: reference @" ++ ref ++ " not found" | ref /= "*"]
getInline (Span (_,["citeproc-no-output"],_) _) =
  ["pandoc-citeproc: reference with no printed form"]
getInline _ = []


getBlock :: Block -> [String]
getBlock (Div (_, ["ppp-err"], [("file", fp)]) _) =
  ["ppp: file " ++ fp ++ " not found"]
getBlock (Div (_, ["ppp-err"], [("macro", src), ("err", err)]) _) =
  ["ppp: unable to parse macro " ++ src ++ "\n" ++ err ++ "\n"]
getBlock (Div (_, ["ppp-err"], [("multiinstance", macro)]) _) =
  ["ppp: multiple instances of macro " ++ macro]
getBlock (Div (_, ["ppp-err"], [("unknown", macro)]) _) =
  ["ppp: unknown macro " ++ macro]
getBlock (Div (_, ["ppp-err"], [("type", doctype)]) _) =
  ["ppp: unknown document type " ++ doctype]
getBlock _ = []

divs :: Block -> [Block]
divs d@(Div _ _) = [d]
divs _ = []

printErrors :: Pandoc -> IO ()
printErrors doc = do
  mapM_ putStrLn . query getBlock $ doc
  mapM_ putStrLn . query getInline $ doc
