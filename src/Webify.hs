{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Exception
import           Control.Monad
import           Data.Binary.Strict.Get
import qualified Data.ByteString        as B
import           Data.Monoid
import           EOT
import           Font                   hiding (str)
import           Options.Applicative    hiding (header)
import           OTF
import           SVG
import           System.FilePath
import           TTF
import           Utils
import           WOFF


-- platformAppleUnicode = 0
-- platformMacintosh = 1
-- platformISO = 2
platformMicrosoft = 3

-- encodingUndefined = 0
encodingUGL = 1

data Opts = Opts { noEot         :: Bool
                 , noWoff        :: Bool
                 , noSvg         :: Bool
                 , enableSvgKern :: Bool
                 , zopfli        :: Bool
                 , svgPlatformId :: UShort
                 , svgEncodingId :: UShort
                 , inputs        :: [String]}

optsDef :: Parser Opts
optsDef = Opts <$> switch (long "no-eot" <> help "Disable eot")
          <*> switch (long "no-woff" <> help "Disable woff")
          <*> switch (long "no-svg" <> help "Disable svg")
          <*> switch (long "svg-enable-kerning" <> help "Enable svg kerning")
          <*> switch (long "zopfli" <> help "Use Zopfli Compression Algorithm")
          <*> option auto (long "svg-cmap-platform-id" <> value platformMicrosoft <> help "Svg cmap platform id")
          <*> option auto (long "svg-cmap-encoding-id" <> value encodingUGL  <> help "Svg cmap encoding id")
          <*> some (argument str (metavar "FONTS"))

changeExtension :: FilePath -> FilePath -> FilePath
changeExtension ext = flip addExtension ext . dropExtension

cmapDescription :: (Integral a) => a -> a -> String
cmapDescription 0 0 = "Unicode 1.0 semantics"
cmapDescription 0 1 = "Unicode 1.1 semantics"
cmapDescription 0 2 = "ISO/IEC 10646 semantics"
cmapDescription 0 3 = "Unicode 2.0 and onwards semantics, Unicode BMP only"
cmapDescription 0 4 = "Unicode 2.0 and onwards semantics, Unicode full repertoire"
cmapDescription 0 5 = "Unicode Variation Sequences"
cmapDescription 0 6 = "Unicode full repertoire"
cmapDescription 0 _ = "Unicode unknown"
cmapDescription 1 0 = "Macintosh Roman 8-bit simple"
cmapDescription 1 _ = "Macintosh unknown"
cmapDescription 3 0 = "Microsoft Symbol"
cmapDescription 3 1 = "Microsoft Unicode BMP (UCS-2)"
cmapDescription 3 2 = "Microsoft ShiftJIS"
cmapDescription 3 3 = "Microsoft PRC"
cmapDescription 3 4 = "Microsoft Big5"
cmapDescription 3 5 = "Microsoft Wansung"
cmapDescription 3 6 = "Microsoft Johab"
cmapDescription 3 7 = "Microsoft Reserved"
cmapDescription 3 8 = "Microsoft Reserved"
cmapDescription 3 9 = "Microsoft Reserved"
cmapDescription 3 10 = "Microsoft Unicode UCS-4"
cmapDescription 3 _ = "Microsoft Unknown"
cmapDescription _ _ = "Unknown"

showCmap :: Cmap -> String
showCmap cmap' =
  formatTable $ header : map pluck (encodingDirectories cmap')
  where header = ["PlatformId", "EncodingId", "Description"]
        pluck dir = [show $ cmapPlatformId dir, show $ cmapEncodingId dir, cmapDescription (cmapPlatformId dir) (cmapEncodingId dir)]

eotgen :: Font f => f -> B.ByteString -> FilePath -> IO ()
eotgen font input filename = do
  putStrLn $ "Generating " ++ target
  B.writeFile target $ EOT.generate font input
  where target = changeExtension "eot" filename

woffgen :: Font f => f -> B.ByteString -> FilePath -> Opts -> IO ()
woffgen font input filename Opts{..}= do
  putStrLn $ "Generating " ++ target
  B.writeFile target $ WOFF.generate font input zopfli
  where target = changeExtension "woff" filename

svggen :: TTF -> B.ByteString -> FilePath -> Opts -> IO ()
svggen ttf input filename Opts{..} = do
  putStrLn $ "Generating " ++ target
  putStrLn "Available cmaps"
  putStr $ showCmap $ cmap ttf
  putStrLn $ "Selecting platformId " ++ show svgPlatformId ++ " encodingId " ++ show svgEncodingId ++ " -- " ++ cmapDescription svgPlatformId svgEncodingId
  B.writeFile target $ SVG.generate ttf input cmapTable enableSvgKern
  where target = changeExtension "svg" filename
        cmapTable = cmapTableFind ttf svgPlatformId svgEncodingId

convert :: Opts -> FilePath -> IO ()
convert opts@Opts{..} filename = do
  input <- B.readFile filename
  if takeExtension filename == ".otf"
    then let otf = fromRight $ runGet (OTF.parse input) input in
    do
      unless noEot (eotgen otf input filename)
      unless noWoff (woffgen otf input filename opts)
    else let ttf = fromRight $ runGet (TTF.parse input) input in
    do
      unless noEot (eotgen ttf input filename)
      unless noWoff (woffgen ttf input filename opts)
      unless noSvg (svggen ttf input filename opts)

convertFiles :: Opts -> IO ()
convertFiles opts@Opts{inputs = fonts} =
  forM_ fonts $ safeConvert opts
  where safeConvert opts' file = convert opts' file `catch` displayError file
        displayError :: FilePath -> SomeException -> IO ()
        displayError file e = (putStrLn $ "Failed to convert " ++ file) >> (putStrLn $ show e)

webifyVersion = "0.1.7.0"

main :: IO ()
main = do
  opts <- execParser optsSpec
  maybe (putStrLn $ "webify " ++ webifyVersion) convertFiles opts
  where
    parser = flag' Nothing (long "version" <> help "Display version") <|> (Just <$> optsDef)
    optsSpec = info (helper <*> parser) mempty

