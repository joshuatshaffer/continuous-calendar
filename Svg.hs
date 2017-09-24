
module Svg where

data SvgElem = Line Double Double Double Double Bool
             | Number Double Double Int
             deriving (Show,Eq)

toSvg :: SvgElem -> String
toSvg (Line x1 y1 x2 y2 isBold) = "<line x1=\"" ++ show x1 ++ "\" y1=\"" ++ show y1 ++ "\" x2=\"" ++ show x2 ++ "\" y2=\"" ++ show y2 ++ "\" style=\"stroke:black;stroke-width:" ++ (if isBold then "1" else "3") ++ "\" />\n"
toSvg (Number x y n) = "<text x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" font-size = \"24\" >" ++ show n ++ "</text>\n"
