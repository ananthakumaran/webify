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
import Font
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


payload :: Font f => f -> B.ByteString -> Put
payload font rawFont = do
  putULong (fromIntegral (B.length rawFont)) -- font size
  putULong 0x00020001
  putULong 0 -- flags
  let name' = name font
  mapM_ putByte $ os2panose font
  putByte 0x01
  putByte (if testBit (os2fsSelection font) 0 then 0x01 else 0)
  putULong $ fromIntegral $ os2usWeightClass font
  putUShort 0  --  embedding permission putUShort $ fsType os
  putUShort 0x504C
  putULong $ os2ulUnicodeRange1 font
  putULong $ os2ulUnicodeRange2 font
  putULong $ os2ulUnicodeRange3 font
  putULong $ os2ulUnicodeRange4 font
  putULong $ os2ulCodePageRange1 font
  putULong $ os2ulCodePageRange2 font
  putULong $ headCheckSumAdjusment font
  replicateM_ 4 (putULong 0)
  putUShort 0
  putNameStr name' 1
  putNameStr name' 2
  putNameStr name' 5
  putNameStr name' 4
  putUShort 0 -- RootString
  putByteString rawFont


combine :: B.ByteString -> PutM ()
combine rest = do
  putULong $ fromIntegral $ B.length rest + 4
  putByteString rest

generate :: Font f => f -> B.ByteString -> B.ByteString
generate font rawFont =
  let rest = toStrict $ runPut (payload font rawFont)
  in toStrict $ runPut $ combine rest
