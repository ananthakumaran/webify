import qualified Data.ByteString as B
import TTF
import EOT
import Data.Binary.Strict.Get
import Utils
import System.Environment (getArgs)
import System.FilePath
import Control.Monad

main :: IO ()
main = do
  inputs <- getArgs
  forM_ inputs convert

addEOTExtension :: FilePath -> FilePath
addEOTExtension = flip addExtension "eot" . dropExtension

convert :: FilePath -> IO ()
convert filename = do
  input <- B.readFile filename
  let ttf = getResult $ (runGet (parse input) $ input)
--  print ttf
  B.writeFile (addEOTExtension filename) $ generate ttf input
