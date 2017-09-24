
module Svg where

data SvgElem = Line Double Double Double Double Bool
             | Number Double Double Int

toSvg :: SvgElem -> String
toSvg (Line x1 y1 x2 y2 isBold) = "<line x1=\"" ++ show x1 ++ "in\" y1=\"" ++ show y1 ++ "in\" x2=\"" ++ show x2 ++ "in\" y2=\"" ++ show y2 ++ "in\" style=\"stroke:black;stroke-width:" ++ (if isBold then "1" else "3") ++ "\" />"
toSvg (Number x y n) = "<text x=\"" ++ show x ++ "in\" y=\"" ++ show y ++ "in\" >" ++ show n ++ "</text>"
