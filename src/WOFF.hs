module WOFF(
  generate
) where

import Codec.Compression.Zlib
import Control.Monad
import Data.Binary.Put
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Data.Map as Map
import Data.Word
import Data.List
import TTF hiding(head)
import Utils
import Data.Function


type UInt32 = Word32
type UInt16 = Word16

putUInt32 :: UInt32 -> Put
putUInt32 = putWord32be

putUInt16 :: UInt16 -> Put
putUInt16 = putWord16be

putTableDirectory ::
  ((Int, Int, Int, B.ByteString), TableDirectory) -> PutM ()
putTableDirectory ((startOffset, size, _padding, _compressedData), directory) = do
  putByteString $ pack $ tag directory
  putUInt32 $ fromIntegral startOffset
  putUInt32 $ fromIntegral size
  putUInt32 $ fromIntegral $ TTF.length directory
  putUInt32 $ checkSum directory


calculateOffset ::
  [(Int, Int, Int, B.ByteString)] -> B.ByteString
  -> [(Int, Int, Int, B.ByteString)]
calculateOffset offsets raw =
  (start, size, padding, compressedData) : offsets
  where originalSize = B.length raw
        compressed = toStrict $ compress $ toLazy raw
        compressedSize = B.length compressed
        compressedData | originalSize <= compressedSize = raw
                       | otherwise = compressed
        size = min originalSize compressedSize
        (lastStart, lastSize, lastPadding, _) = head offsets
        start = lastStart + lastSize + lastPadding
        padding | (size `mod` 4) == 0 = 0
                | otherwise = 4 - (size `mod` 4)

putFontData :: (Int, Int, Int, B.ByteString) -> PutM ()
putFontData (_, _, padding, compressedData) = do
  putByteString compressedData
  replicateM_ padding (putWord8 0x0)

payload :: TTF -> B.ByteString -> Put
payload ttf font = do
  putUInt16 $ numTables ttf
  putUInt16 0 -- reserved
  putUInt32 $ fromIntegral $ B.length font
  putUInt16 1 -- woff version major
  putUInt16 0 -- woff version minor
  putUInt32 0 -- meta offset
  putUInt32 0 -- meta length
  putUInt32 0 -- meta length uncompressed
  putUInt32 0 -- private block offset
  putUInt32 0 -- private block length
  let tds = Map.elems $ tableDirectories ttf
      sortByOffset = sortBy (compare `on` offset)
      sortedByTag = sortBy (compare `on` tag . snd)
      initialOffset = [(fromIntegral (44 + (20 * numTables ttf)), 0, 0, pack "")]
      offsets = drop 1 $ reverse $
                foldl calculateOffset initialOffset (map rawData $ sortByOffset tds)
  mapM_ putTableDirectory $ sortedByTag $ zip offsets (sortByOffset tds)
  mapM_ putFontData offsets


combine :: TTF -> B.ByteString -> PutM ()
combine ttf rest = do
  putUInt32 0x774F4646
  putUInt32 $ version ttf
  putUInt32 $ fromIntegral $ B.length rest + 12
  putByteString rest

generate :: TTF -> B.ByteString -> B.ByteString
generate ttf font =
  let rest = toStrict $ runPut (payload ttf font)
  in toStrict $ runPut $ combine ttf rest
