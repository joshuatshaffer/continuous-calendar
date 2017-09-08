module Main where

import Data.Time.Calendar

docWidth,docHeight :: Double
docWidth = 6.5
docHeight = 9
dayWidth,dayHeight :: Double
dayWidth = docWidth / 7
dayHeight = docHeight / 6

tomorrow :: Day -> Day
tomorrow = addDays 1

splitWeeks :: [a] -> [[a]]
splitWeeks xs = w:splitWeeks r
  where (w,r) = splitAt 7 xs

daysOnPage :: Day -> [[Day]]
daysOnPage = take 6 . splitWeeks . iterate tomorrow

sameMonth :: Day -> Day -> Bool
sameMonth x y = u == v
  where
    (_,u,_) = toGregorian x
    (_,v,_) = toGregorian y

dbh :: Double -> Double -> (Double, Double, Double, Double)
dbh x y = (x * dayWidth, (y + 1) * dayHeight
          ,(x + 1) * dayWidth, (y + 1) * dayHeight)

dbv :: Double -> Double -> (Double, Double, Double, Double)
dbv x y = ((x+1) * dayWidth, y * dayHeight
          ,(x + 1) * dayWidth, (y + 1) * dayHeight)

hlines :: [[Day]] -> [((Double, Double, Double, Double), Bool)]
hlines days = [(dbh x y, dayAt x y `sameMonth` dayAt x (y+1)) | x <- [0..6], y <- [0..4]]
  where
    dayAt x y = (days !! round y) !! round x

vlines :: [[Day]] -> [((Double, Double, Double, Double), Bool)]
vlines days = [(dbv x y, dayAt x y `sameMonth` dayAt (x+1) y) | x <- [0..5], y <- [0..5]]
  where
    dayAt x y = (days !! round y) !! round x

numbers :: [[Day]] -> [((Double,Double),Int)]
numbers days = [(pos x y, dayAt x y) | x <- [0..6], y <- [0..5]]
  where
    dayAt x y = (\(_,_,n)->n) $ toGregorian ((days !! round y) !! round x)
    pos x y = (x * dayWidth + 0.05, y * dayHeight + 0.12)

lineToSVG :: ((Double, Double, Double, Double), Bool) -> String
lineToSVG ((x1,y1,x2,y2),isBold) = "<line x1=\"" ++ show x1 ++ "in\" y1=\"" ++ show y1 ++ "in\" x2=\"" ++ show x2 ++ "in\" y2=\"" ++ show y2 ++ "in\" style=\"stroke:black;stroke-width:" ++ (if isBold then "1" else "3") ++ "\" />"

numberToSVG :: ((Double,Double),Int) -> String
numberToSVG ((x,y),n) = "<text x=\"" ++ show x ++ "in\" y=\"" ++ show y ++ "in\" >" ++ show n ++ "</text>"

main :: IO ()
main = do
  let days = daysOnPage $ fromGregorian 2017 8 10
  mapM_ print days
  let bor = [((0,0,0,docHeight),True),((0,0,docWidth,0),True),((docWidth,0,docWidth,docHeight),True),((0,docHeight,docWidth,docHeight),True)]
  let ls = map lineToSVG (vlines days ++ hlines days ++ bor)
  let ns = map numberToSVG (numbers days)
  let b = concat (ns ++ ls)
  writeFile "cal.svg" $ "<svg height=\"9in\" width=\"6.5in\">" ++ b ++ "</svg>"
