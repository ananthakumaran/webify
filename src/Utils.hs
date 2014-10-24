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
) where

import           Control.Monad
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Function
import           Data.List
import           Debug.Trace
import           Text.Printf

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

debug :: Monad m => String -> m ()
debug str =
  when True $ trace str $ return ()
