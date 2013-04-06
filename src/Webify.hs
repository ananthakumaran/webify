
import Control.Monad
import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import EOT
import SVG
import System.Environment (getArgs)
import System.FilePath
import TTF
import Utils
import WOFF

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
  B.writeFile (changeExtension "eot" filename) $ EOT.generate ttf input
  B.writeFile (changeExtension "woff" filename) $ WOFF.generate ttf input
  B.writeFile (changeExtension "svg" filename) $ SVG.generate ttf input
