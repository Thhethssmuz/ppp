{-# LANGUAGE TemplateHaskell #-}

module Emb (emb) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir)

emb :: [(FilePath, BS.ByteString)]
emb = $(embedDir "data")
