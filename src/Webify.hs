import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import TTF
import EOT
import Data.Binary.Strict.Get

getResult ((Right x), _) = x

main :: IO ()
main = do
  input <- B.getContents
  let ttf = getResult $ (runGet (parse input) $ input)
--  print ttf
  B.putStr $ generate ttf input

