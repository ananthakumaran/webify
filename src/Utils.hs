module Utils(
  getResult
  , substr
  , toStrict
  , toLazy
  , diff
  , debug
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Debug.Trace


getResult :: (Either String t2, t) -> t2
getResult ((Right x), _) = x
getResult ((Left y), _) = error y

substr :: Int -> Int -> B.ByteString -> B.ByteString
substr s l = B.take l . B.drop s

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

toLazy :: B.ByteString -> BL.ByteString
toLazy str = BL.fromChunks  [str]

diff :: Num b => [b] -> [b]
diff l = map (\(a, b) -> a - b) $ zip l $ tail l

debug str =
  when True $ trace str $ return ()
