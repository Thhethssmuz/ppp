module PpP.Err (printErrors) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk


getInline :: Inline -> [String]
getInline (Span (_,["citeproc-not-found"],[("data-reference-id",ref)]) _) =
  ["pandoc-citeproc: reference @" ++ ref ++ " not found" | ref /= "*"]
getInline (Span (_,["citeproc-no-output"],_) _) =
  ["pandoc-citeproc: reference with no printed form"]
getInline _ = []


getBlock :: Block -> [String]
getBlock (Div (_, ["ppp-err"], [("err", err)]) _) = ["ppp: " ++ err]
getBlock _ = []


printErrors :: Pandoc -> IO ()
printErrors doc = do
  mapM_ putStrLn . query getBlock $ doc
  mapM_ putStrLn . query getInline $ doc
