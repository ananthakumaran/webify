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
  , OS2(..)
  , Head(..)


  , parseTableDirectory
  , parseTableDirectories
  , parseTable
  , parseOS2
  , parseHead
  , parseName
) where

import Control.Monad
import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack)
import Data.Int
import Data.Map hiding(map)
import qualified Data.Text as T
import Data.Text.Encoding.Error
import qualified Data.Vector as V
import Data.Word
import Utils
import Data.Text.Encoding

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

data OS2 = OS2 { os2Version :: UShort
               , xAvgCharWidth :: Short
               , usWeightClass :: UShort
               , usWidthClass :: UShort
               , fsType :: UShort
               , ySubscriptXSize :: Short
               , ySubscriptYSize :: Short
               , ySubscriptXOffset :: Short
               , ySubscriptYOffset :: Short
               , ySuperscriptXSize :: Short
               , ySuperscriptYSize :: Short
               , ySuperscriptXOffset :: Short
               , ySuperscriptYOffset :: Short
               , yStrikeoutSize :: Short
               , yStrikeoutPosition :: Short
               , sFamilyClass :: Short
               , panose :: [Byte]
               , ulUnicodeRange1 :: ULong
               , ulUnicodeRange2 :: ULong
               , ulUnicodeRange3 :: ULong
               , ulUnicodeRange4 :: ULong
               , aschVendID :: [Byte]
               , fsSelection :: UShort
               , usFirstCharIndex :: UShort
               , usLastCharIndex :: UShort
               , sTypoAscender :: UShort
               , sTypoDescender :: UShort
               , sTypoLineGap :: UShort
               , usWinAscent :: UShort
               , usWinDescent :: UShort
               , ulCodePageRange1 :: ULong
               , ulCodePageRange2 :: ULong
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

parseOS2 :: Map String TableDirectory -> B.ByteString -> OS2
parseOS2 = parseTable "OS/2" (do
  os2Version <- getUShort
  unless (os2Version `elem` [1..4]) (error $ "unhandled  os2 version " ++ show os2Version)
  xAvgCharWidth <- getShort
  usWeightClass <- getUShort
  usWidthClass <- getUShort
  fsType <- getUShort
  ySubscriptXSize <- getShort
  ySubscriptYSize <- getShort
  ySubscriptXOffset <- getShort
  ySubscriptYOffset <- getShort
  ySuperscriptXSize <- getShort
  ySuperscriptYSize <- getShort
  ySuperscriptXOffset <- getShort
  ySuperscriptYOffset <- getShort
  yStrikeoutSize <- getShort
  yStrikeoutPosition <- getShort
  sFamilyClass <- getShort
  panose <- replicateM 10 getByte
  ulUnicodeRange1 <- getULong
  ulUnicodeRange2 <- getULong
  ulUnicodeRange3 <- getULong
  ulUnicodeRange4 <- getULong
  aschVendID <- replicateM 4 getByte
  fsSelection <- getUShort
  usFirstCharIndex <- getUShort
  usLastCharIndex <- getUShort
  sTypoAscender <- getUShort
  sTypoDescender <- getUShort
  sTypoLineGap <- getUShort
  usWinAscent <- getUShort
  usWinDescent <- getUShort
  ulCodePageRange1 <- getULong
  ulCodePageRange2 <- getULong
  return OS2 {..})

data Head = Head { headVersion :: Fixed
                 , fontRevision :: Fixed
                 , checkSumAdjusment :: ULong
                 , magicNumber :: ULong
                 , headFlags :: UShort
                 , unitsPerEm :: UShort
                 , created :: B.ByteString
                 , modified :: B.ByteString
                 , xMin :: FWord
                 , yMin :: FWord
                 , xMax :: FWord
                 , yMax :: FWord
                 , macStyle :: UShort
                 , lowestRecPPEM :: UShort
                 , fontDirectionHint :: Short
                 , indexToLocFormat :: Short
                 , glyphDataFormat :: Short
                 } deriving (Show)

parseHead :: Map String TableDirectory -> B.ByteString -> Head
parseHead = parseTable "head" (do
  headVersion <- getFixed
  fontRevision <- getFixed
  checkSumAdjusment <- getULong
  magicNumber <- getULong
  headFlags <- getUShort
  unitsPerEm <- getUShort
  created <- getByteString 8
  modified <- getByteString 8
  xMin <- getFWord
  yMin <- getFWord
  xMax <- getFWord
  yMax <- getFWord
  macStyle <- getUShort
  lowestRecPPEM <- getUShort
  fontDirectionHint <- getShort
  indexToLocFormat <- getShort
  glyphDataFormat <- getShort
  return Head{..})

parseNameRecord :: B.ByteString -> Int -> Get NameRecord
parseNameRecord font storageOffset = do
  platformId <- getUShort
  encodingId <- getUShort
  languageId <- getUShort
  nameId <- getUShort
  strLength <- getUShort
  strOffset <- getUShort
  let str = decoder platformId encodingId $ substr
            (fromIntegral ((fromIntegral storageOffset :: Int) + fromIntegral strOffset)) (fromIntegral strLength) font
  return NameRecord{..}
  where
    decoder 3 _ = decodeUtf16BE
    decoder 2 _ = decodeUtf16BE
    decoder 1 _ = decodeUtf8With ignore
    decoder 0 _ = decodeUtf16BE
    decoder _ _ = decodeUtf16BE

parseName ::Map String TableDirectory -> B.ByteString -> Name
parseName tds font =
  fromRight $ runGet (do
    let tableStart = fromIntegral $ tDOffset $ tds ! "name"
    skip tableStart
    formatSelector <- getUShort
    numberOfNameRecords <- getUShort
    storageOffset <- getUShort
    nameRecords <- replicateM (fromIntegral numberOfNameRecords) $ parseNameRecord font (tableStart + fromIntegral storageOffset)
    return Name{..}) font

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
