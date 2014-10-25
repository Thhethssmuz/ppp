module PpP.Filter.Wikiref (wikiref, wikiref') where

import Data.Ord (comparing)
import Data.Char (isAlphaNum, isSpace)
import Data.List (intersperse, findIndices, sortBy)
import Data.Maybe (fromMaybe, listToMaybe)

import Text.Pandoc
import Text.Pandoc.Walk
import Text.CSL.Pandoc
import qualified Text.CSL as CSL

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Lazy as M



trimWith :: (a -> Bool) -> [a] -> [a]
trimWith p = f . f where f = reverse . dropWhile p

toString :: Inline -> String
toString Space   = " "
toString (Str s) = s

sanitize :: [Inline] -> String
sanitize = trimWith (not . isAlphaNum) . concatMap toString

toCite :: Citation -> Inline
toCite (Citation i p s m n h) = 
  Cite [Citation i p' s' m n h] $ p' ++ i' ++ s'
  where p' = if sanitize p == "" then []
                                 else [Str $ sanitize p, Space]
        s' = if sanitize s == "" then []
                                 else [Str ",", Space, Str $ sanitize s]
        i' = [Str $ "@" ++ i]

splitCitation :: Inline -> Inline
splitCitation (Cite cs is) = Span ("",["wiki-ref-inline"],[]) . map toCite $ cs
splitCitation x = x



genNormal :: Inline -> Inline
genNormal (Cite [c] is) = 
  Span ("",["tmp"],[]) [
    Cite [ c ] is,
    Cite [ c {citationMode = NormalCitation} ] is
  ]
genNormal x = x

replNormal :: Inline -> Inline
replNormal (Span ("",["tmp"],[]) [Cite c no, Cite _ full]) =
  Cite c . evalState (walkM replaceNotes no) . query notes $ full
replNormal x = x

replaceNotes :: Inline -> State [Inline] Inline
replaceNotes (Note bs) = do
  ns <- get
  case ns of
    (n:ns') -> do
               put ns'
               return n
    []      -> return (Note bs)
replaceNotes x = return x

replaceCites :: Inline -> State [Inline] Inline
replaceCites (Cite c is) = do
  cs <- get
  case cs of
    ((Cite c' is'):cs') -> do
                           put cs'
                           return $ Cite c' is'
    []                  -> return $ Cite c is
replaceCites x = return x



label :: String -> Inline
label s = RawInline (Format "tex") $ "{\\label{" ++ s ++ "}}"

mkWikiRef :: Int -> Int -> Inline
mkWikiRef i b = Span ("note-back-ref-" ++ index ++ letter, [], [("ref", index)]) $
  [ Link [Superscript [Str $ "[" ++ index ++ "]"]] ("#note-ref-" ++ index, "") ] ++
  [ label $ "note-back-ref-" ++ index ++ letter ]
  where index  = show i
        letter = (: "") . (['a'..] !!) $ b

noteSlice :: Inline -> ([Inline], [Block])
noteSlice (Note ((Para is):bs)) = (is, bs)
noteSlice (Note bs) = ([], bs)

mkWikiNote :: Int -> Int -> Inline -> [Block]
mkWikiNote i 1 n = 
  [ Para $ [
      Span ("note-ref-" ++ index, [], []) $ [
        Link [Strong [Str "^"]] ("#note-back-ref-" ++ index ++ "a", ""),
        Space
      ] ++ is ++ [label $ "note-ref-" ++ index]
    ]
  ] ++ bs
  where index    = show i
        (is, bs) = noteSlice n
mkWikiNote i r n = 
  [ Para [
      Span ("note-ref-" ++ index, [], []) $
      [Str "^", Space] ++ refs ++ [Space] ++ is ++ [label $ "note-ref-" ++ index]
    ]
  ] ++ bs
  where index    = show i
        (is, bs) = noteSlice n
        refs     = intersperse Space 
                 . map (mkBackRef)
                 . map (:"")
                 . take r $ ['a'..]
        mkBackRef letter = Link [Superscript [Strong [Str letter]]]
                                ("#note-back-ref-" ++ index ++ letter, "")



buildNotes :: [Inline] -> Inline -> State (M.Map Int Int) Inline
buildNotes ns n@(Note bs) = do 
  let ref = minimum . findIndices (== n) $ ns
  letter <- gets (fromMaybe 0 . M.lookup ref)
  modify $ M.insert ref (letter + 1)
  return $ mkWikiRef (ref+1) letter
buildNotes _ x = return x


refnum :: Inline -> [Int]
refnum (Span (rid,[],[("ref", n)]) _) =
  case take 13 rid of
    "note-back-ref" -> [read n]
    _               -> []
refnum _ = []

numberCitation :: Inline -> Inline
numberCitation (Cite [c] is) =
  Cite [c{citationHash = fromMaybe (-1) . listToMaybe . query refnum $ is}] is
numberCitation x = x

orderCites :: Inline -> Inline
orderCites (Span ("",["wiki-ref-inline"],[]) cs) =
    Span ("", [], []) 
  . sortBy (comparing (citationHash . (\(Cite [c] _) -> c)))
  . walk numberCitation $ cs
orderCites x = x



insertDiv :: String -> Block -> Block -> Block
insertDiv s bs x@(Div (did, [s'], []) _) = 
  if s == s' then Div (did, [s],[]) [bs]
             else x
insertDiv _ _ x = x



notes :: Inline -> [Inline]
notes n@(Note _) = [n]
notes _ = []

citations :: Inline -> [Inline]
citations c@(Cite _ _) = [c]
citations _ = []

referencediv :: Block -> [Block]
referencediv (Div (_,["references"],[]) bs) = bs
referencediv _ = []



wikiref :: CSL.Style -> [CSL.Reference] -> Pandoc -> Pandoc
wikiref style bib pandoc@(Pandoc meta blocks) =
  let doc       = walk splitCitation $ pandoc
      shadowdoc = walk replNormal
                . processCites style bib
                . Pandoc meta . (:[]) . Para
                . walk genNormal 
                . query citations $ doc

      doc'      = evalState (walkM replaceCites doc)
                . query citations $ shadowdoc

      notes'    = query notes doc'
      walker    = walkM (buildNotes notes') doc'
      state     = execState walker $ M.fromList []
      doc''     = evalState walker $ M.fromList []
      noteslist = OrderedList (1, Decimal, Period)
                . map (uncurry . uncurry $ mkWikiNote)
                . map (\(i,(r,rs)) -> ((i, rs), notes' !! r))
                . zip [1..]
                . M.toList $ state
      biblist   = BulletList
                . map (:[])
                . query referencediv $ shadowdoc
      doc'''    = walk orderCites
                . walk (insertDiv "notes" noteslist)
                . walk (insertDiv "bibliograhpy" biblist)
                $ doc''
  in  doc'''

wikiref' :: Pandoc -> IO Pandoc
wikiref' pandoc@(Pandoc meta blocks) = do
  let doc       = walk splitCitation $ pandoc
  
  shadowdoc    <- fmap (walk replNormal)
                . processCites'
                . Pandoc meta . (:[]) . Para
                . walk genNormal 
                . query citations $ doc

  let doc'      = evalState (walkM replaceCites doc)
                . query citations $ shadowdoc

      notes'    = query notes doc'
      walker    = walkM (buildNotes notes') doc'
      state     = execState walker $ M.fromList []
      doc''     = evalState walker $ M.fromList []
      noteslist = OrderedList (1, Decimal, Period)
                . map (uncurry . uncurry $ mkWikiNote)
                . map (\(i,(r,rs)) -> ((i, rs), notes' !! r))
                . zip [1..]
                . M.toList $ state
      biblist   = BulletList
                . map (:[])
                . query referencediv $ shadowdoc
      doc'''    = walk orderCites
                . walk (insertDiv "notes" noteslist)
                . walk (insertDiv "bibliography" biblist)
                $ doc''
  return doc'''
