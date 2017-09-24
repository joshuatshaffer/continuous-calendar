module Main (main) where

import Data.Time.Calendar
import Svg
import CalMake
import CleanUp

drawPage :: Day -> IO ()
drawPage d = do
  let b = concatMap toSvg . cleanUpSvg 1300 1800 . makePage 1300 1800 6 $ d
  writeFile ("cal-" ++ show d ++ ".svg") $ "<svg height=\"11in\" width=\"8.5in\" viewBox=\"-200 -200 1700 2200\">\n" ++ b ++ "</svg>\n"

main :: IO ()
main = do
  let startDays = iterate (addDays (6*7)) $ fromGregorian 2017 5 14
  mapM_ drawPage $ take 6 startDays
