module SVG(
  generate
) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Char
import Data.List (find, foldl')
import Data.Maybe (fromJust)
import qualified Data.Text as T
import TTF
import Text.XML.Generator
import Numeric
import Utils
import Data.Vector as V ((!), length, last)

byNameId :: UShort -> TTF -> T.Text
byNameId id' ttf =
  str $ fromJust $ find predicate $ nameRecords $ name ttf
  where predicate nr =
          nameId nr == id'

fontFamilyName :: TTF -> T.Text
fontFamilyName = byNameId 1

svgDocType :: String
svgDocType = "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" >"

svgDocInfo :: DocInfo
svgDocInfo = DocInfo { docInfo_standalone = False
                     , docInfo_docType = Just svgDocType
                     , docInfo_preMisc = xempty
                     , docInfo_postMisc = xempty }


advanceX :: Hmtx -> Int -> UFWord
advanceX hmtx' id' =
  advanceWidth hMetric
  where hMetrics' = hMetrics hmtx'
        hMetric | id' < V.length hMetrics' = hMetrics' ! id'
                | otherwise = V.last hMetrics'


contourPath :: [(Double, Double, Int)] -> String
contourPath contour =
  "M" ++ show x' ++ " " ++ show y' ++ path 0 ccontour
  where (x', y', _) = Prelude.head contour
        show x | (fromIntegral floored) == x = Prelude.show floored
               | otherwise = formatFloat 1 x
          where floored = floor x :: Int
        formatFloat precision a = showFFloat (Just precision) a ""
        midval :: Double -> Double -> Double
        midval a b = a + (b - a) / 2
        onCurve flag = testBit flag 0
        ccontour = cycle contour
        second = Prelude.head . tail
        third = Prelude.head . tail . tail
        path n ccontour' | n >= Prelude.length contour = "Z"
                   | otherwise =
                     let (x, y, f) = Prelude.head ccontour'
                         (x1, y1, f1) = second ccontour'
                         (x2, y2, f2) = third ccontour'
                         next True True _ | x == x1 = ("V" ++ show y1) ++ rest
                                          | y == y1 = ("H" ++ show x1) ++ rest
                                          | otherwise = ("L" ++ show x1 ++ " " ++ show y1) ++ rest
                           where rest = path (n + 1) (tail ccontour')
                         next True False True = ("Q" ++ show x1 ++ " " ++ show y1 ++ " " ++ show x2 ++ " " ++ show y2) ++ rest
                           where rest = path (n + 2) (drop 2 ccontour')
                         next True False False = ("Q" ++ show x1 ++ " " ++ show y1 ++ " " ++ show  (midval x1 x2) ++ " " ++ show (midval y1 y2)) ++ rest
                           where rest = path (n + 2) (drop 2 ccontour')
                         next False False _ = ("T" ++ show (midval x x1) ++ " " ++ show (midval y y1)) ++ rest
                           where rest = path (n + 1) (tail ccontour')
                         next False True _ = ("T" ++ show x1 ++ " " ++ show y1) ++ rest
                           where rest = path (n + 1) (tail ccontour')
                         -- rest not implemented
                     in
                      next (onCurve f) (onCurve f1) (onCurve f2)


glyphPoints :: Glyf -> TTF -> [[(Double, Double, Int)]]
glyphPoints EmptyGlyf _ = []
glyphPoints CompositeGlyf{cGlyfs = cglyfs} ttf =
  concatMap cglyhPoints cglyfs
  where
    cglyhPoints cg = map (map (transform cg)) (glyphPoints (glyf $ cGlyphIndex cg) ttf)
    glyf index = glyfs ttf ! fromIntegral index
    transform cg (x, y, flag) =
      ((x * cXScale cg + y * cScale10 cg) + fromIntegral (cXoffset cg)
      , (y * cYScale cg + x * cScale01 cg) + fromIntegral (cYoffset cg)
      , flag)

glyphPoints glyph _ =
  reverse bpts
  where endPts = map fromIntegral $ sEndPtsOfCountours glyph
        pts = zipWith3 (\x y f -> (fromIntegral x, fromIntegral y, fromIntegral f)) (sXCoordinates glyph) (sYCoordinates glyph) (sFlags glyph)
        splitPts (ac, offset', pts') x = (take (x - offset') pts' : ac, x, drop (x - offset') pts')
        (bpts, _, _) = foldl' splitPts ([], -1, pts) endPts


svgPath :: Glyf -> TTF -> String
svgPath glyf ttf = foldl' (++) "" $ map contourPath (glyphPoints glyf ttf)


escapeXMLChar :: Char -> String
escapeXMLChar '<' = "&lt;"
escapeXMLChar '>' = "&gt;"
escapeXMLChar '&' = "&amp;"
escapeXMLChar '"' = "&quot;"
escapeXMLChar '\'' = "&#39;"
escapeXMLChar c | oc <= 0x7f && isPrint c = [c]
                | otherwise = "&#x" ++ showHex oc "" ++ ";"
  where oc = ord c

svgGlyph :: TTF -> CmapTable -> Int -> Int -> Xml Elem
svgGlyph ttf cmapTable averageAdvanceX code =
  xelem "glyph" (xattrs ([xattrRaw "unicode" (escapeXMLChar $ chr code)] ++
                         horizAdvanceXAttr ++
                         [xattrRaw "d" $ svgPath glyph ttf]))
  where glyphId' = glyphId cmapTable code
        glyph = glyfs ttf ! glyphId'
        horizAdvanceX = fromIntegral $ advanceX (hmtx ttf) glyphId'
        horizAdvanceXAttr | horizAdvanceX == averageAdvanceX = []
                          | otherwise = [xattrRaw "horiz-adv-x" $ show horizAdvanceX]


missingGlyph :: TTF -> Xml Elem
missingGlyph ttf  =
  xelem "missing-glyph" (xattrs [xattr "horiz-adv-x" $ show $ advanceX (hmtx ttf) 0,
                                 xattr "d" $ svgPath glyph ttf])
  where glyph = glyfs ttf ! 0


validCharCode :: Int -> Bool
validCharCode n | n == 0x9 = True
                | n == 0xA = True
                | n == 0xD = True
                | n >= 0x20 && n <= 0xD7FF = True
                | n >= 0xE000 && n <= 0xFFFD = True
                | n >= 0x1000 && n <= 0x10FFFF = True
                | otherwise = False

svgGlyphs :: TTF -> CmapTable -> (Xml Elem, Int)
svgGlyphs ttf cmapTable=
  (xelems $ missingGlyph ttf : map (svgGlyph ttf cmapTable averageAdvanceX) validGlyphCodes, averageAdvanceX)
  where codeRange = [(cmapStart cmapTable)..(cmapEnd cmapTable)]
        validGlyph code = validCharCode code && glyphId cmapTable code > 0
        validGlyphCodes = filter validGlyph codeRange
        averageAdvanceX = maxDuplicate $ map (fromIntegral . advanceX (hmtx ttf) . glyphId cmapTable) validGlyphCodes

fontFace :: TTF -> Xml Elem
fontFace ttf =
  xelem "font-face" (xattrs [xattr "font-family" $ fontFamilyName ttf,
                             xattr "units-per-em" $ show . unitsPerEm $ TTF.head ttf,
                             xattr "ascent" $ show . ascender $ hhea ttf,
                             xattr "descent" $ show . descender $ hhea ttf])


testText :: TTF -> Xml Elem
testText ttf =
  xelem "g" (xattr "style" ("font-family: " ++ show (fontFamilyName ttf) ++ "; font-size:50;fill:black") <#>
             xelems (zipWith text ["!\"#$%&'()*+,-./0123456789:;å<>?",
                                   "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_",
                                   "` abcdefghijklmnopqrstuvwxyz|{}~"] [1..]))
  where
    text :: String -> Int -> Xml Elem
    text t i = xelem "text" (xattrs [xattr "x" "20",
                                     xattr "y" $ show (i * 50)] <#> xtext t)

svgbody :: TTF -> CmapTable -> Xml Elem
svgbody ttf cmapTable =
  xelems [xelemEmpty "metadata",
          xelem "defs" $
          xelem "font" (xattrs [xattr "horiz-adv-x" $ show avgAdvanceX] <#>
                        xelems [fontFace ttf,
                                glyps]),
          testText ttf]
  where (glyps, avgAdvanceX) = svgGlyphs ttf cmapTable

generate :: TTF -> B.ByteString -> CmapTable -> B.ByteString
generate ttf _font cmapTable =
  xrender svg
  where
    svg = doc svgDocInfo $
          xelem "svg" (xattr "xmlns" "http://www.w3.org/2000/svg" <#> svgbody ttf cmapTable)
