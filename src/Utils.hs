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
  , xattrRaw
  , xattr
) where

import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Monad
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy               as BL
import           Data.Function
import           Data.List
import           Data.Text                          ()
import qualified Data.Text                          as T
import           Debug.Trace
import           Text.Printf
import qualified Text.XML.Generator                 as X

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

xattrRaw :: String -> String -> X.Xml X.Attr
xattrRaw name attr = X.xattrQRaw X.defaultNamespace (T.pack name) (fromString attr)

xattr :: String -> T.Text -> X.Xml X.Attr
xattr name = X.xattr (T.pack name)
