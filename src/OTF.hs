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

instance Font OTF where
  version = OTF.version
  numTables = OTF.numTables
  tableDirectories = OTF.tableDirectories
  os2panose = panose . os2
  os2fsSelection = fsSelection . os2
  os2usWeightClass = usWeightClass . os2
  os2ulUnicodeRange1 = ulUnicodeRange1 . os2
  os2ulUnicodeRange2 = ulUnicodeRange2 . os2
  os2ulUnicodeRange3 = ulUnicodeRange3 . os2
  os2ulUnicodeRange4 = ulUnicodeRange4 . os2
  os2ulCodePageRange1 = ulCodePageRange1 . os2
  os2ulCodePageRange2 = ulCodePageRange2 . os2
  headCheckSumAdjusment = checkSumAdjusment . OTF.head
  name = OTF.name

data OTF = OTF { version :: Fixed
               , numTables :: UShort
               , searchRange :: UShort
               , entrySelector :: UShort
               , rangeShift :: UShort
               , tableDirectories :: Map String TableDirectory
               , os2 :: OS2
               , head :: Head
               , name :: Name
               } deriving (Show)

parse :: B.ByteString -> Get OTF
parse font = do
  version <- getFixed
  numTables <- getUShort
  searchRange <- getUShort
  entrySelector <- getUShort
  rangeShift <- getUShort
  tableDirectories <- parseTableDirectories font (fromIntegral numTables)
  let os2 = parseOS2 tableDirectories font
      head = parseHead tableDirectories font
      name = parseName tableDirectories font
  return OTF{..}
