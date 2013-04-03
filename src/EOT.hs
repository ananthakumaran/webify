module EOT(
  generate
) where

import TTF
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Word
import Control.Monad
import Data.Bits
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Text.Encoding (encodeUtf16LE)
import Data.Binary.Put


type ULong = Word32
type UShort = Word16

putULong = putWord32le
putByte = putWord8
putUShort = putWord16le


match nameId' name =
  fromJust $ find predicate records
  where
    records = nameRecords name
    predicate nr =
      platformId nr == 3 &&
      encodingId nr == 1 &&
      languageId nr == 0x0409 &&
      nameId nr == nameId'

putNameStr name i = do
  let nameRecord = match i name
  putUShort $ strLength nameRecord
  putByteString $ encodeUtf16LE $ str nameRecord
  putUShort 0

payload :: TTF -> B.ByteString -> Put
payload ttf font = do
  putULong (fromIntegral (B.length font)) -- font size
  putULong 0x00020001
  putULong 0 -- flags
  let os = os2 ttf
      head = TTF.head ttf
      name = TTF.name ttf
  mapM_ putByte $ panose os
  putByte 0x01
  putByte (if (testBit (fsSelection os) 0) then 0x01 else 0)
  putULong $ fromIntegral $ usWeightClass os
  putUShort 0  --  embedding permission putUShort $ fsType os
  putUShort 0x504C
  putULong $ ulUnicodeRange1 os
  putULong $ ulUnicodeRange2 os
  putULong $ ulUnicodeRange3 os
  putULong $ ulUnicodeRange4 os
  putULong $ ulCodePageRange1 os
  putULong $ ulCodePageRange2 os
  putULong $ checkSumAdjusment head
  _ <- replicateM 4 (putULong 0)
  putUShort 0
  putNameStr name 1
  putNameStr name 2
  putNameStr name 5
  putNameStr name 4
  putUShort 0 -- RootString
  putByteString font


combine rest = do
  putULong $ fromIntegral $ B.length rest + 4
  putByteString rest

generate :: TTF -> B.ByteString -> B.ByteString
generate ttf font =
  let rest = B.concat $ BL.toChunks $ runPut (payload ttf font)
  in B.concat $ BL.toChunks $ runPut $ combine rest
