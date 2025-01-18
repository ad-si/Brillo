{-# LANGUAGE OverloadedStrings #-}

{-| Tree Fractal.
     Based on ANUPlot code by Clem Baker-Finch.
-}
module Main where

import Brillo (
  Color,
  Display (InWindow),
  Picture (Color, Pictures, Polygon, Rotate, Scale, Translate),
  animate,
  black,
  dim,
  green,
  makeColorI,
  mixColors,
 )


main :: IO ()
main =
  animate
    (InWindow "Tree" (500, 650) (20, 20))
    black
    (picture 4)


-- The picture is a tree fractal, graded from brown to green
picture :: Int -> Float -> Picture
picture degree time =
  Translate 0 (-300) $
    tree degree time (dim $ dim brown)


-- Basic stump shape
stump :: Color -> Picture
stump colr =
  Color colr $
    Polygon [(30, 0), (15, 300), (-15, 300), (-30, 0)]


-- Make a tree fractal.
tree
  :: Int -- Fractal degree
  -> Float -- time
  -> Color -- Color for the stump
  -> Picture
tree 0 _time colr = stump colr
tree n time colr =
  let smallTree =
        Rotate (sin time) $
          Scale 0.5 0.5 $
            tree (n - 1) (-time) (greener colr)
  in  Pictures
        [ stump colr
        , Translate 0 300 smallTree
        , Translate 0 240 $ Rotate 20 smallTree
        , Translate 0 180 $ Rotate (-20) smallTree
        , Translate 0 120 $ Rotate 40 smallTree
        , Translate 0 60 $ Rotate (-40) smallTree
        ]


-- | Starting color for the stump
brown :: Color
brown = makeColorI 139 100 35 255


-- | Make the color a little greener
greener :: Color -> Color
greener =
  mixColors 1 10 green
