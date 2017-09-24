module CleanUp where

import Svg (SvgElem(Line))
import Data.List (nub)
import Data.Maybe (mapMaybe)

cleanUpSvg :: Double -> Double -> [SvgElem] -> [SvgElem]
cleanUpSvg pageWidth pageHeight = replaceBorder pageWidth pageHeight . removeRedundent

removeRedundent :: [SvgElem] -> [SvgElem]
removeRedundent = filter (not . zeroLen) . nub
  where
    zeroLen (Line x1 y1 x2 y2 _) = (x1,y1) == (x2,y2)
    zeroLen _ = False

replaceBorder :: Double -> Double -> [SvgElem] -> [SvgElem]
replaceBorder pageWidth pageHeight = (++ [Line 0 0 0 pageHeight False, Line pageWidth 0 pageWidth pageHeight False]) . mapMaybe foo
  where
    foo l@(Line x1 _ x2 _ _)
      | x1 == 0 && x2 == 0 = Nothing
      | x1 == pageWidth && x2 == pageWidth = Nothing
      | otherwise = Just l
    foo x = Just x
