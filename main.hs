module Main (main) where

import Data.Time.Calendar

data SvgElem = Line Double Double Double Double Bool
             | Number Double Double Int

toSvg :: SvgElem -> String
toSvg (Line x1 y1 x2 y2 isBold) = "<line x1=\"" ++ show x1 ++ "in\" y1=\"" ++ show y1 ++ "in\" x2=\"" ++ show x2 ++ "in\" y2=\"" ++ show y2 ++ "in\" style=\"stroke:black;stroke-width:" ++ (if isBold then "1" else "3") ++ "\" />"
toSvg (Number x y n) = "<text x=\"" ++ show x ++ "in\" y=\"" ++ show y ++ "in\" >" ++ show n ++ "</text>"

docWidth,docHeight :: Double
docWidth = 6.5
docHeight = 9
dayWidth,dayHeight :: Double
dayWidth = docWidth / 7
dayHeight = docHeight / 6

tomorrow,yesterday,nextWeek,lastWeek :: Day -> Day
tomorrow = addDays 1
yesterday = addDays (-1)
nextWeek = addDays 7
lastWeek = addDays (-7)

daysOnPage :: Day -> [Day]
daysOnPage = take (6*7) . iterate tomorrow

sameMonth :: Day -> Day -> Bool
sameMonth x y = monthX == monthY
  where
    (_,monthX,_) = toGregorian x
    (_,monthY,_) = toGregorian y

dayBox :: Day -> (Double,Double) -> [SvgElem]
dayBox d (x1,y1) = [top, bottom, left, right, num]
  where x2 = x1 + dayWidth
        y2 = y1 + dayHeight
        top    = Line x1 y1 x2 y1 (d `sameMonth` lastWeek d)
        bottom = Line x1 y2 x2 y2 (d `sameMonth` nextWeek d)
        left   = Line x1 y1 x1 y2 (d `sameMonth` yesterday d)
        right  = Line x2 y1 x2 y2 (d `sameMonth` tomorrow d)
        num = let (_,_,n) = toGregorian d
              in Number (x1 + 0.05) (y1 + 0.12) n

dayPos :: Day -> Day -> (Double,Double)
dayPos firstDay d = (x,y)
  where n = diffDays d firstDay
        x = fromInteger (n `mod` 7) * dayWidth
        y = fromInteger (n `div` 7) * dayHeight

elemsOfPage :: [Day] -> [SvgElem]
elemsOfPage days = concatMap (\d -> dayBox d $ dayPos (head days) d) days

main :: IO ()
main = do
  let b = concatMap toSvg . elemsOfPage . daysOnPage $ fromGregorian 2017 9 10
  writeFile "cal.svg" $ "<svg height=\"9in\" width=\"6.5in\">" ++ b ++ "</svg>"
