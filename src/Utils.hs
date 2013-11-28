{-# LANGUAGE RecordWildCards #-}
module Utils(
  fromRight
  , substr
  , toStrict
  , toLazy
  , diff
  , debug
  , formatTable
  , maxDuplicate

    -- parser
  , Byte
  , Utils.Char
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
  , Utils.getChar
  , getFWord
  , getUFWord
  , getUFixed

    -- common font structure
  , TableDirectory(..)
  , parseTableDirectory
  , parseTableDirectories
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (unpack)
import Control.Monad
import Debug.Trace
import Data.List
import Text.Printf
import Data.Function
import Data.Word
import Data.Int
import Data.Binary.Strict.Get
import Data.Map hiding(map, findIndex)

fromRight :: (Either String t2, t) -> t2
fromRight (Right x, _) = x
fromRight (Left y, _) = error y

substr :: Int -> Int -> B.ByteString -> B.ByteString
substr s l = B.take l . B.drop s

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

toLazy :: B.ByteString -> BL.ByteString
toLazy str = BL.fromChunks  [str]

diff :: Num b => [b] -> [b]
diff l = zipWith (-) l $ tail l

formatTable :: [[String]] -> String
formatTable table =
  line ++ unlines (map formatRow table) ++ line
  where maxSize = map maximum $ transpose $ map (map length) table
        line = concat (replicate (sum maxSize + (length maxSize - 1) * 3) "-") ++ "\n"
        formatRow row = intercalate " | " $ zipWith formatCell row maxSize
        formatCell :: String -> Int -> String
        formatCell field size = printf ("%-" ++ show size ++ "s") field

maxDuplicate :: [Int] -> Int
maxDuplicate = head . minimumBy (compare `on` (negate . length)) . group . sort

debug str =
  when True $ trace str $ return ()


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

getChar :: Get Utils.Char
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
