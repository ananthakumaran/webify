module Utils(
  getResult
  , substr
) where

import qualified Data.ByteString as B

getResult :: (Either t1 t2, t) -> t2
getResult ((Right x), _) = x

substr :: Int -> Int -> B.ByteString -> B.ByteString
substr s l = B.take l . B.drop s
