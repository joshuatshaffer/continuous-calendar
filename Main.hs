module Main (main) where

import Data.Time.Calendar
import Svg
import CalMake

main :: IO ()
main = do
  let b = concatMap toSvg . makePage 6.5 9 6 $ fromGregorian 2017 9 10
  writeFile "cal.svg" $ "<svg height=\"9in\" width=\"6.5in\">" ++ b ++ "</svg>"
