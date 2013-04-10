import Control.Monad
import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import EOT
import SVG
import System.FilePath
import TTF hiding(str)
import Utils
import WOFF
import Options.Applicative
import Data.Monoid


data Opts = Opts { noEot :: Bool
                 , noWoff :: Bool
                 , noSvg :: Bool
                 , inputs :: [String]}

optsDef :: Parser Opts
optsDef = Opts <$> switch (long "no-eot" <> help "Disable eot")
          <*> switch (long "no-woff" <> help "Disable woff")
          <*> switch (long "no-svg" <> help "Disable eot")
          <*> arguments1 str (metavar "FONTS")

changeExtension :: FilePath -> FilePath -> FilePath
changeExtension ext = flip addExtension ext . dropExtension

convert :: Opts -> FilePath -> IO ()
convert Opts{noEot = noEot, noWoff = noWoff, noSvg = noSvg} filename = do
  input <- B.readFile filename
  let ttf = getResult $ runGet (parse input) input
  unless noEot (gen "eot" $ EOT.generate ttf input)
  unless noWoff (gen "woff" $ WOFF.generate ttf input)
  unless noSvg (gen "svg" $ SVG.generate ttf input)
    where gen ext = B.writeFile (changeExtension ext filename)


convertFiles :: Opts -> IO ()
convertFiles opts@Opts{inputs = fonts} =
  forM_ fonts $ convert opts

main :: IO ()
main = execParser opts >>= convertFiles
  where
    parser = helper <*> optsDef
    opts = info parser mempty
