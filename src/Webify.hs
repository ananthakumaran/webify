import qualified Data.ByteString as B
import TTF
import EOT
import WOFF
import Data.Binary.Strict.Get
import Utils
import System.Environment (getArgs)
import System.FilePath
import Control.Monad

main :: IO ()
main = do
  inputs <- getArgs
  forM_ inputs convert

changeExtension :: FilePath -> FilePath -> FilePath
changeExtension ext = flip addExtension ext . dropExtension

convert :: FilePath -> IO ()
convert filename = do
  input <- B.readFile filename
  let ttf = getResult $ (runGet (parse input) $ input)
--  print ttf
  B.writeFile (changeExtension "eot" filename) $ EOT.generate ttf input
  B.writeFile (changeExtension "woff" filename) $ WOFF.generate ttf input
