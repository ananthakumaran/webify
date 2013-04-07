module SVG(
  generate
) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Char
import Data.List (find, findIndex)
import Data.Maybe (fromJust)
import TTF
import Text.XML.Generator

byNameId id ttf =
  str $ fromJust $ find predicate $ nameRecords $ name ttf
  where predicate nr =
          nameId nr == id

fontFamilyName = byNameId 1

cmapTableFind ttf platformId encodingId =
  (subTables $ cmap ttf) !! index
  where directories = encodingDirectories $ cmap ttf
        index = fromJust $ findIndex predicate directories
        predicate dr = cmapPlatformId dr == platformId &&
                       cmapEncodingId dr == encodingId

svgDocType = "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" >"

svgDocInfo :: DocInfo
svgDocInfo = DocInfo { docInfo_standalone = False
                     , docInfo_docType = Just svgDocType
                     , docInfo_preMisc = xempty
                     , docInfo_postMisc = xempty }


advanceX hmtx id =
  advanceWidth hMetric
  where hMetrics' = hMetrics hmtx
        hMetric | id < Prelude.length hMetrics' = (hMetrics' !! id)
                | otherwise = last hMetrics'




contourPath :: [(Int, Int, Int)] -> String
contourPath contour =
  "M" ++ show x ++ " " ++ show y ++ path 0 ""
  where (x, y, _) = Prelude.head contour
        midval a b = (fromIntegral a) + (fromIntegral (b - a)) / 2
        onCurve flag = testBit flag 0
        ccontour = cycle contour
        path n acc | n >= (Prelude.length contour) = acc ++ "Z"
                   | otherwise =
                     let (x, y, f) = ccontour !! n
                         (x1, y1, f1) = ccontour !! (n + 1)
                         (x2, y2, f2) = ccontour !! (n + 2)
                         next True True _ | x == x1 = path (n + 1) (acc ++ "V" ++ show y1)
                                          | y == y1 = path (n + 1) (acc ++ "H" ++ show x1)
                                          | otherwise = path (n + 1) (acc ++ "L" ++ show x1 ++ " " ++ show y1)
                         next True False True = path (n + 2) (acc ++ "Q" ++ show x1 ++ " " ++ show y1 ++ " " ++ show x2 ++ " " ++ show y2)
                         next True False False = path (n + 2) (acc ++ "Q" ++ show x1 ++ " " ++ show y1 ++ " " ++ (show $ midval x1 x2) ++ " " ++ (show $ midval y1 y2))
                         next False False _ = path (n + 1) (acc ++ "T" ++ (show $ midval x x1) ++ " " ++ (show $ midval y y1))
                         next False True _ = path (n + 1) (acc ++ "T" ++ show x1 ++ " " ++ show y1)
                         -- rest not implemented
                     in
                      next (onCurve f) (onCurve f1) (onCurve f2)


svgPath :: Glyf -> String
svgPath EmptyGlyf = ""
svgPath glyph =
  body
  where endPts = sEndPtsOfCountours glyph
        pts = zip3 (map fromIntegral $ sXCoordinates glyph) (map fromIntegral $ sYCoordinates glyph) (map fromIntegral $ sFlags glyph)
        splitPts (ac, offset', pts') x = (ac ++ [(take (x - offset') pts')], x, drop (x - offset') pts')
        (bpts, _, _) = foldl splitPts ([], -1, (pts)) $ map fromIntegral endPts
        body = foldl (++) "" (map contourPath (bpts))




svgGlyph ttf cmapTable code =
  xelem "glyph" (xattrs [xattr "unicode" $ [chr code],
                         xattr "horiz-adv-x" $ show $ advanceX (hmtx ttf) glyphId',
                         xattr "d" $ svgPath glyph
                        ])
  where glyphId' = glyphId cmapTable code
        glyph = glyfs ttf !! glyphId'


missingGlyph ttf cmapTable =
  xelem "missing-glyph" (xattrs [xattr "horiz-adv-x" $ show $ advanceX (hmtx ttf) 0,
                                 xattr "d" $ svgPath glyph])
  where glyph = glyfs ttf !! 0

svgGlyphs ttf =
  let cmapTable = cmapTableFind ttf 1 0
      start = fromJust $ findIndex ((<) 0) $ c0glyphIDs cmapTable
  in xelems $ missingGlyph ttf cmapTable : map (svgGlyph ttf cmapTable) [32..126]

fontFace ttf =
  xelem "font-face" (xattrs [xattr "font-family" $ fontFamilyName ttf,
                             xattr "units-per-em" $ show . unitsPerEm $ TTF.head ttf,
                             xattr "ascent" $ show . ascender $ hhea ttf,
                             xattr "descent" $ show . descender $ hhea ttf])


testText ttf =
  xelem "g" (xattr "style" ("font-family: " ++ (show $ fontFamilyName ttf) ++ "; font-size:50;fill:black") <#>
             xelems (map text $ zip ["!\"#$%&'()*+,-./0123456789:;<>?",
                                     "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_",
                                     "` abcdefghijklmnopqrstuvwxyz|{}~"] [1..]))
  where
    text :: (String, Int) -> Xml Elem
    text (t, i) = xelem "text" (xattrs [xattr "x" "20",
                                     xattr "y" $ show (i * 50)] <#> xtext t)


svgbody ttf =
  xelems [xelemEmpty "metadata",
          (xelem "defs" $
           xelem "font" (xattrs [xattr "horiz-adv-x" $ show $ xAvgCharWidth $ os2 ttf] <#>
                         xelems [fontFace ttf,
                                 svgGlyphs ttf])),
          testText ttf]



generate :: TTF -> B.ByteString -> B.ByteString
generate ttf font =
  let svg = doc svgDocInfo $
            xelem "svg" (xattr "xmlns" "http://www.w3.org/2000/svg" <#> svgbody ttf)
  in xrender svg
