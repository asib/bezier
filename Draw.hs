module Draw
( lineEquation
, gradient
, drawLine
) where

import Point
import Line
import Debug.Trace

-- First  is    0 < m <= 1
-- Second is    1 < m <= inf
-- Third  is   -1 < m <= 0
-- Fourth is -inf < m <= -1
data Quadrant = First | Second | Third | Fourth deriving (Show)

lineEquation :: Line -> Point -> Double
lineEquation (Line (x0,y0) (x1,y1)) (x,y) =
  (y0-y1)*x + (x1-x0)*y + x0*y1 - x1*y0

gradient :: Line -> Double
gradient (Line (x0,y0) (x1,y1)) =
  (y1-y0)/(x1-x0)

getQuadrant :: Line -> Quadrant
getQuadrant l@(Line (x0,y0) (x1,y1))
  | 0 < g && g <= 1 = First
  | 1 < g && g <= (1/0) = Second -- 1/0 is Infinity
  | -1 < g && g <= 0 = Third
  | otherwise = Fourth
  where g = gradient l

nextDecisionPoint :: Line -> Quadrant -> Double -> Double -> Double -> (Double,Double,Double,Double)
nextDecisionPoint l@(Line (x0,y0) (x1,y1)) First a b d =
  if d < 0 then (a,b,b+1,d+(x1-x0)+(y0-y1))
           else (a,b,b,d+(y0-y1))
nextDecisionPoint l@(Line (x0,y0) (x1,y1)) Second a b d =
  if d > 0 then (b,a,b+1,d+(x1-x0)+(y0-y1))
           else (b,a,b,d+(x1-x0))
nextDecisionPoint l@(Line (x0,y0) (x1,y1)) Third a b d =
  if d > 0 then (a,b,b-1,d-(x1-x0)+(y0-y1))
           else (a,b,b,d+(y0-y1))
nextDecisionPoint l@(Line (x0,y0) (x1,y1)) Fourth a b d =
  if d < 0 then (b,a,b+1,d-(x1-x0)+(y0-y1))
           else (b,a,b,d-(x1-x0))

midpointLoop :: Line -> Quadrant -> [Double] -> Double -> Double -> [Pixel]
midpointLoop _ _ [] _ _ = []
midpointLoop l@(Line (x0,y0) (x1,y1)) q (a:as) b d =
  (round x,round y) : midpointLoop l q as b' d'
  where (x,y,b',d') = nextDecisionPoint l q a b d

drawLine :: Line -> [Pixel]
drawLine l@(Line (x0,y0) (x1,y1))
  | x0 <= x1  = midpointLoop l quad [a0, a0'..a1] b0 d
  | otherwise = drawLine (Line (x1,y1) (x0,y0))
  where dFunc = lineEquation l
        quad  = getQuadrant l
        ((a0,a0',a1), b0, d) = case quad of
                             First  -> ((x0,x0+1,x1),y0,dFunc (x0+1,y0+0.5))
                             Second -> ((y0,y0+1,y1),x0,dFunc (x0+0.5,y0+1))
                             Third  -> ((x0,x0+1,x1),y0,dFunc (x0+1,y0-0.5))
                             Fourth -> ((y0,y0-1,y1),x0,dFunc (x0+0.5,y0-1))
