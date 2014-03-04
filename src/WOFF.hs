module WOFF(
  generate
) where

import qualified Codec.Compression.Hopfli as Hopfli
import qualified Codec.Compression.Zlib   as Zlib
import           Control.Monad
import           Data.Binary.Put
import qualified Data.ByteString          as B
import           Data.ByteString.Char8    (pack)
import           Data.Function
import           Data.List
import qualified Data.Map                 as Map
import           Data.Word
import           Font
import           Utils


type UInt32 = Word32
type UInt16 = Word16

putUInt32 :: UInt32 -> Put
putUInt32 = putWord32be

putUInt16 :: UInt16 -> Put
putUInt16 = putWord16be

putTableDirectory ::
  ((Int, Int, Int, B.ByteString), TableDirectory) -> PutM ()
putTableDirectory ((startOffset, size, _padding, _compressedData), directory) = do
  putByteString $ pack $ tDTag directory
  putUInt32 $ fromIntegral startOffset
  putUInt32 $ fromIntegral size
  putUInt32 $ fromIntegral $ tDLength directory
  putUInt32 $ tDCheckSum directory


type Compressor = B.ByteString -> B.ByteString

getCompressor :: Bool -> Compressor
getCompressor False = toStrict . Zlib.compress . toLazy
getCompressor True = Hopfli.compress

calculateOffset ::
  Compressor ->
  [(Int, Int, Int, B.ByteString)] -> B.ByteString
  -> [(Int, Int, Int, B.ByteString)]
calculateOffset compressor offsets raw =
  (start, size, padding, compressedData) : offsets
  where originalSize = B.length raw
        compressed = compressor raw
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

payload :: Font f => f -> B.ByteString -> Bool -> Put
payload font rawFont enableZopfli = do
  putUInt16 $ numTables font
  putUInt16 0 -- reserved
  putUInt32 $ fromIntegral $ B.length rawFont
  putUInt16 1 -- woff version major
  putUInt16 0 -- woff version minor
  putUInt32 0 -- meta offset
  putUInt32 0 -- meta length
  putUInt32 0 -- meta length uncompressed
  putUInt32 0 -- private block offset
  putUInt32 0 -- private block length
  let tds = Map.elems $ tableDirectories font
      sortByOffset = sortBy (compare `on` tDOffset)
      sortedByTag = sortBy (compare `on` tDTag . snd)
      initialOffset = [(fromIntegral (44 + (20 * numTables font)), 0, 0, pack "")]
      offsets = drop 1 $ reverse $
                foldl (calculateOffset $ getCompressor enableZopfli) initialOffset (map tDRawData $ sortByOffset tds)
  mapM_ putTableDirectory $ sortedByTag $ zip offsets (sortByOffset tds)
  mapM_ putFontData offsets


combine :: Font f => f -> B.ByteString -> PutM ()
combine font rest = do
  putUInt32 0x774F4646
  putUInt32 $ version font
  putUInt32 $ fromIntegral $ B.length rest + 12
  putByteString rest

generate :: Font f => f -> B.ByteString -> Bool -> B.ByteString
generate font rawFont enableZopfli =
  let rest = toStrict $ runPut (payload font rawFont enableZopfli)
  in toStrict $ runPut $ combine font rest
