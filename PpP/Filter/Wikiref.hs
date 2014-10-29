module PpP.Filter.Wikiref (wikiref, wikiref') where

import Data.Ord (comparing)
import Data.Char (isAlphaNum, isSpace)
import Data.List (intersperse, findIndices, sortBy)
import Data.Maybe (fromMaybe, listToMaybe, isJust)

import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Shared (stringify)
import Text.CSL.Pandoc
import qualified Text.CSL as CSL

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Lazy as M

import Paths_ppp
import System.FilePath
import System.Directory (doesFileExist)


trimWith :: (a -> Bool) -> [a] -> [a]
trimWith p = f . f where f = reverse . dropWhile p

sanitize :: [Inline] -> String
sanitize = trimWith (not . isAlphaNum) . concatMap stringify

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
insertDiv s bs x@(Div (did, cs, [("name", s')]) _) = 
  if s == s' then Div (did, cs, [("name", s)]) [bs]
             else x
insertDiv _ _ x = x



notes :: Inline -> [Inline]
notes n@(Note _) = [n]
notes _ = []

citations :: Inline -> [Inline]
citations c@(Cite _ _) = [c]
citations _ = []

referenceDiv :: Block -> [Block]
referenceDiv (Div (_,["references"],[]) bs) = bs
referenceDiv _ = []



citeproc :: CSL.Style -> [CSL.Reference] -> Pandoc -> Pandoc
citeproc style bib doc@(Pandoc meta _) =
  let shadowdoc = processCites style bib
                . Pandoc meta . (:[]) . Para
                . query citations $ doc
      doc'      = evalState (walkM replaceCites doc)
                . query citations $ shadowdoc
      biblist   = Div ("",[],[])
                . query referenceDiv $ shadowdoc
  in              walk (insertDiv "bibliography" biblist) doc'

wikiref :: CSL.Style -> [CSL.Reference] -> Pandoc -> Pandoc
wikiref style bib pandoc@(Pandoc meta _) =
  let noteStyle = (==) "note" . take 4 . CSL.styleClass $ style
      doc       = walk splitCitation pandoc
      shadowdoc = (if noteStyle then walk replNormal else id)
                . processCites style bib
                . Pandoc meta . (:[]) . Para
                . (if noteStyle then walk genNormal else id)
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
                . query referenceDiv $ shadowdoc
  in              walk orderCites
                . walk (insertDiv "notes" noteslist)
                . walk (insertDiv "bibliography" biblist) $ doc''



wikiref' :: Pandoc -> IO Pandoc
wikiref' pandoc@(Pandoc meta _) = do
  defcsl  <- getDataFileName $ "csl" </> "wikiref.csl"

  csl     <- CSL.readCSLFile ( fmap stringify'
                             . lookupMeta "locale" $ meta )
           . fromMaybe defcsl
           . fmap stringify'
           . lookupMeta "csl" $ meta

  bib     <- maybe (return []) CSL.readBiblioFile
           . fmap stringify'
           . lookupMeta "bibliography" $ meta

  return   $ case isJust . lookupMeta "wikiref" $ meta of
    True  -> wikiref csl bib $ pandoc
    False -> citeproc csl bib $ pandoc



stringify' :: MetaValue -> String
stringify' (MetaString s) = s
stringify' (MetaInlines is) = stringify is
