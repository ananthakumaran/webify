{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}

module OTF(
  OTF(..)
  , parse
) where

import qualified Data.ByteString as B
import Data.Binary.Strict.Get
import Data.Map
import Font

data OTF = OTF { version :: Fixed
               , numTables :: UShort
               , searchRange :: UShort
               , entrySelector :: UShort
               , rangeShift :: UShort
               , tableDirectories :: Map String TableDirectory
               } deriving (Show)

parse :: B.ByteString -> Get OTF
parse font = do
  version <- getFixed
  numTables <- getUShort
  searchRange <- getUShort
  entrySelector <- getUShort
  rangeShift <- getUShort
  tableDirectories <- parseTableDirectories font (fromIntegral numTables)
  return OTF{..}
