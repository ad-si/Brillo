module Main where

import Brillo.Data.Color
import Brillo.Data.Picture
import Brillo.Export


main :: IO ()
main =
  exportPicturesToGif
    10
    LoopingForever
    (160, 160)
    white
    "inf_loop.gif"
    (animation . (* a))
    [0 .. (steps - 1)]
  where
    steps = 40
    a = (90) / steps
    animation t = Rotate t (poly 50)


poly :: Float -> Picture
poly l = Polygon [(-l, l), (l, l), (l, -l), (-l, -l)]
