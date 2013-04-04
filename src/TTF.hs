{-# LANGUAGE RecordWildCards #-}

module TTF(
  TTF(..)
  , OS2(..)
  , Head(..)
  , Name(..)
  , NameRecord(..)
  , parse
) where

import Data.Map.Strict hiding(map)
import Data.Binary.Strict.Get
import Data.ByteString.Char8 (unpack)
import Data.Word
import Data.Int
import Control.Monad
import Utils
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16BE)
import qualified Data.ByteString as B


type Byte = Word8
type Char = Int8
type UShort = Word16
type Short = Int16
type ULong = Word32
type Long = Int32
type Fixed = Int32

getFixed :: Get Fixed
getFixed = liftM fromIntegral getWord32be

getULong :: Get ULong
getULong = getWord32be

getUShort :: Get UShort
getUShort = getWord16be

getShort :: Get Short
getShort = liftM fromIntegral getWord16be

data TableDirectory = TableDirectory { tag :: String
                                     , checkSum :: ULong
                                     , offset :: ULong
                                     , length :: ULong
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

data Head = Head { headVersion :: Fixed
                 , fontRevision :: Fixed
                 , checkSumAdjusment :: ULong
                 , magicNumber :: ULong
                 } deriving (Show)

data TTF = TTF { version :: Fixed
               , numTables :: UShort
               , searchRange :: UShort
               , entrySelector :: UShort
               , rangeShift :: UShort
               , tableDirectories :: Map String TableDirectory
               , os2 :: OS2
               , head :: Head
               , name :: Name
               } deriving (Show)


parseTableDirectory :: Get(TableDirectory)
parseTableDirectory = do
  tag <- liftM unpack $ getByteString 4
  checkSum <- getULong
  offset <- getULong
  length <- getULong
  return TableDirectory{..}

parseTableDirectories :: Int -> Get (Map String TableDirectory)
parseTableDirectories n = do
  list <- replicateM n parseTableDirectory
  return $ fromList $ map (\x -> (tag x, x)) list

parseNameRecord :: B.ByteString -> Int -> Get NameRecord
parseNameRecord font storageOffset = do
  platformId <- getUShort
  encodingId <- getUShort
  languageId <- getUShort
  nameId <- getUShort
  strLength <- getUShort
  strOffset <- getUShort
  let str = decodeUtf16BE $ substr (fromIntegral ((fromIntegral storageOffset) + (fromIntegral strOffset))) (fromIntegral strLength) font
  return NameRecord{..}

parseName ::Map String TableDirectory -> B.ByteString -> Name
parseName tableDirectories font =
  getResult $ runGet (do
    let tableStart = fromIntegral $ offset $ tableDirectories ! "name"
    skip $ tableStart
    formatSelector <- getUShort
    numberOfNameRecords <- getUShort
    storageOffset <- getUShort
    nameRecords <- replicateM (fromIntegral numberOfNameRecords) $ parseNameRecord font (tableStart + (fromIntegral storageOffset))
    return Name{..}) font

parseOS2 :: Map String TableDirectory -> B.ByteString -> OS2
parseOS2 = parseTable "OS/2" (do
  os2Version <- getUShort
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
  panose <- replicateM 10 getWord8
  ulUnicodeRange1 <- getULong
  ulUnicodeRange2 <- getULong
  ulUnicodeRange3 <- getULong
  ulUnicodeRange4 <- getULong
  aschVendID <- replicateM 4 getWord8
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


parseHead :: Map String TableDirectory -> B.ByteString -> Head
parseHead = parseTable "head" (do
  headVersion <- getFixed
  fontRevision <- getFixed
  checkSumAdjusment <- getULong
  magicNumber <- getULong
  return Head{..})

parseTable :: String -> Get a -> (Map String TableDirectory -> B.ByteString -> a)
parseTable name m =
  \tableDirectories font ->
  getResult $ runGet (do
    skip $ fromIntegral $ offset $ tableDirectories ! name
    m) font

parse :: B.ByteString -> Get TTF
parse font = do
  version <- getFixed
  numTables <- getUShort
  searchRange <- getUShort
  entrySelector <- getUShort
  rangeShift <- getUShort
  tableDirectories <- parseTableDirectories (fromIntegral numTables)
  let os2 = parseOS2 tableDirectories font
      head = parseHead tableDirectories font
      name = parseName tableDirectories font
    in
    return TTF{..}
