module Image
( Image(Image)
, zero
, setPixel
, getPixel
) where

import Data.Array
import Data.Ix
import Data.Word
import Point

type Red = Word8
type Blue = Word8
type Green = Word8
type ColorBuffer = Array Pixel (Red,Green,Blue)

data Image = Image { width :: Int
                   , height :: Int
                   , colour :: ColorBuffer
                   } deriving (Show)

zero :: Int -> Int -> Image
zero w h =
  Image {width=w
        ,height=h
        ,colour=listArray ((0,0),(w-1,h-1)) (take (w*h) $ repeat (0,0,0))}

setPixel :: Image -> Pixel -> (Red,Green,Blue) -> Image
setPixel img pos c =
  Image {width=w, height=h, colour=(colBuf // [(pos,c)])}
  where colBuf = colour img
        w = width img
        h = height img

getPixel :: Image -> Pixel -> (Red,Green,Blue)
getPixel img pos =
  colour img ! pos
