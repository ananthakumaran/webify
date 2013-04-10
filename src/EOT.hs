module EOT(
  generate
) where

import Control.Monad
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.Text.Encoding (encodeUtf16LE)
import TTF
import Utils

putULong :: ULong -> Put
putULong = putWord32le

putByte :: Byte -> Put
putByte = putWord8

putUShort :: UShort -> Put
putUShort = putWord16le

match :: UShort -> Name -> Maybe NameRecord
match nameId' name' =
  find predicate records
  where
    records = nameRecords name'
    predicate nr =
      ((platformId nr == 1 &&
        encodingId nr == 0 &&
        languageId nr == 0) ||
       (platformId nr == 3 &&
        encodingId nr == 1 &&
        languageId nr == 0x0409)) &&
      nameId nr == nameId'

putNameStr :: Name -> UShort -> PutM ()
putNameStr name' i | isJust mnameRecord = do
  putUShort $ fromIntegral $ B.length encodedStr
  putByteString encodedStr
  putUShort 0
                   | otherwise = do
  putUShort 0
  putUShort 0
  where mnameRecord = match i name'
        nameRecord = fromJust mnameRecord
        encodedStr = encodeUtf16LE $ str nameRecord


payload :: TTF -> B.ByteString -> Put
payload ttf font = do
  putULong (fromIntegral (B.length font)) -- font size
  putULong 0x00020001
  putULong 0 -- flags
  let os = os2 ttf
      head' = TTF.head ttf
      name' = TTF.name ttf
  mapM_ putByte $ panose os
  putByte 0x01
  putByte (if testBit (fsSelection os) 0 then 0x01 else 0)
  putULong $ fromIntegral $ usWeightClass os
  putUShort 0  --  embedding permission putUShort $ fsType os
  putUShort 0x504C
  putULong $ ulUnicodeRange1 os
  putULong $ ulUnicodeRange2 os
  putULong $ ulUnicodeRange3 os
  putULong $ ulUnicodeRange4 os
  putULong $ ulCodePageRange1 os
  putULong $ ulCodePageRange2 os
  putULong $ checkSumAdjusment head'
  replicateM_ 4 (putULong 0)
  putUShort 0
  putNameStr name' 1
  putNameStr name' 2
  putNameStr name' 5
  putNameStr name' 4
  putUShort 0 -- RootString
  putByteString font


combine :: B.ByteString -> PutM ()
combine rest = do
  putULong $ fromIntegral $ B.length rest + 4
  putByteString rest

generate :: TTF -> B.ByteString -> B.ByteString
generate ttf font =
  let rest = toStrict $ runPut (payload ttf font)
  in toStrict $ runPut $ combine rest
