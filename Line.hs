module Line
( Line(Line),
  IntLine(IntLine),
) where

import Point

data Line = Line Point Point deriving (Show)
data IntLine = IntLine Pixel Pixel deriving (Show)
