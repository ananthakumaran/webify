import qualified Data.ByteString as B
import TTF
import EOT
import Data.Binary.Strict.Get
import Utils

main :: IO ()
main = do
  input <- B.getContents
  let ttf = getResult $ (runGet (parse input) $ input)
--  print ttf
  B.putStr $ generate ttf input

