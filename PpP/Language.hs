module PpP.Language where

import qualified Data.Map.Lazy as M

languages :: M.Map String String
languages = M.fromList [
  ("english",  "en-US"),
  ("british",  "en-GB"),
  ("american", "en-US"),
  ("norsk",    "nb-NO"),
  ("nynorsk",  "nn-NO")
  ]

