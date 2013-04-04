module Utils(
  getResult
  , substr
  , toStrict
  , toLazy
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

getResult :: (Either t1 t2, t) -> t2
getResult ((Right x), _) = x

substr :: Int -> Int -> B.ByteString -> B.ByteString
substr s l = B.take l . B.drop s

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

toLazy :: B.ByteString -> BL.ByteString
toLazy str = BL.fromChunks  [str]
