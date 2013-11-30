{-# LANGUAGE RecordWildCards #-}

module Font(
  Font(..)

  -- parser
  , Byte
  , Font.Char
  , UShort
  , Short
  , FWord
  , UFWord
  , ULong
  , Fixed
  , getFixed
  , getULong
  , getUShort
  , getShort
  , getByte
  , Font.getChar
  , getFWord
  , getUFWord
  , getUFixed

    -- common font structure
  , TableDirectory(..)
  , Name(..)
  , NameRecord(..)
  , HMetric(..)
  , Hmtx(..)


  , parseTableDirectory
  , parseTableDirectories
  , parseTable
) where

import Utils
import Data.Map hiding(map)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack)
import Control.Monad
import Data.Word
import Data.Int
import Data.Binary.Strict.Get
import qualified Data.Text as T
import qualified Data.Vector as V

-- font data types

type Byte = Word8
type Char = Int8
type UShort = Word16
type Short = Int16
type FWord = Int16
type UFWord = Word16
type ULong = Word32
-- type Long = Int32
type Fixed = Word32


getFixed :: Get Fixed
getFixed = liftM fromIntegral getWord32be

getULong :: Get ULong
getULong = getWord32be

getUShort :: Get UShort
getUShort = getWord16be

getShort :: Get Short
getShort = liftM fromIntegral getWord16be

getByte :: Get Byte
getByte = getWord8

getChar :: Get Font.Char
getChar = liftM fromIntegral getWord8

getFWord :: Get FWord
getFWord = getShort

getUFWord :: Get UFWord
getUFWord = getUShort

-- format 2.14
getUFixed :: Get Double
getUFixed = liftM ((/ 0x4000) . fromIntegral) getShort


--- common parsing utils
data TableDirectory = TableDirectory { tDTag :: String
                                     , tDCheckSum :: ULong
                                     , tDOffset :: ULong
                                     , tDLength :: ULong
                                     , tDRawData :: B.ByteString
                                     } deriving (Show)

data NameRecord = NameRecord { platformId :: UShort
                               , encodingId :: UShort
                               , languageId :: UShort
                               , nameId :: UShort
                               , strLength :: UShort
                               , strOffset :: UShort
                               , str :: T.Text
                             } deriving (Show)

data Name = Name { formatSelector :: UShort
                   , numberOfNameRecords :: UShort
                   , storageOffset :: UShort
                   , nameRecords :: [NameRecord]
                 } deriving (Show)

data HMetric = HMetric { advanceWidth :: UFWord
                         , lsb :: FWord
                         } deriving (Show)

data Hmtx = Hmtx { hMetrics :: V.Vector HMetric
                 , leftSideBearings :: [FWord]
                 } deriving (Show)

parseTableDirectory :: B.ByteString -> Get TableDirectory
parseTableDirectory font = do
  tDTag <- liftM unpack $ getByteString 4
  tDCheckSum <- getULong
  tDOffset <- getULong
  tDLength <- getULong
  let tDRawData = substr (fromIntegral tDOffset) (fromIntegral tDLength) font
  return TableDirectory{..}

parseTableDirectories :: B.ByteString -> Int -> Get (Map String TableDirectory)
parseTableDirectories font n = do
  list <- replicateM n $ parseTableDirectory font
  return $ fromList $ map (\x -> (tDTag x, x)) list

parseTable :: String -> Get a -> Map String TableDirectory -> B.ByteString -> a
parseTable name' m tds font =
  fromRight $ runGet (do
    skip $ fromIntegral . tDOffset $ tds ! name'
    m) font


class Font a where
  version :: a -> Fixed
  numTables :: a -> UShort
  tableDirectories :: a -> Map String TableDirectory

-- os2
  os2panose :: a -> [Byte]
  os2fsSelection :: a -> UShort
  os2usWeightClass :: a -> UShort
  os2ulUnicodeRange1 :: a -> ULong
  os2ulUnicodeRange2 :: a -> ULong
  os2ulUnicodeRange3 :: a -> ULong
  os2ulUnicodeRange4 :: a -> ULong
  os2ulCodePageRange1 :: a -> ULong
  os2ulCodePageRange2 :: a -> ULong

-- head
  headCheckSumAdjusment :: a -> ULong

-- name
  name :: a -> Name
