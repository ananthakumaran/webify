module Utils(
  getResult
  , substr
  , toStrict
  , toLazy
  , diff
  , debug
  , formatTable
  , maxDuplicate
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Debug.Trace
import Data.List
import Text.Printf
import Data.Function


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

formatTable :: [[String]] -> String
formatTable table =
  line ++ (intercalate "\n" $ map formatRow table) ++ "\n" ++ line
  where maxSize = map maximum $ transpose $ map (map length) table
        line = (foldl' (++) "" $ replicate (sum maxSize + (length maxSize - 1) * 3) "-") ++ "\n"
        formatRow row = intercalate " | " $ zipWith formatCell row maxSize
        formatCell :: String -> Int -> String
        formatCell field size = printf ("%-" ++ show size ++ "s") field

maxDuplicate :: [Int] -> Int
maxDuplicate = head . head . sortBy (compare `on` (negate . length)) . group . sort

debug str =
  when True $ trace str $ return ()
