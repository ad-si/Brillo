{-# LANGUAGE OverloadedStrings #-}

{-| Example demonstrating polygon rendering, including both convex and
non-convex (complex) polygons. Brillo automatically handles non-convex
polygons using GLU tessellation.
-}
module Main where

import Brillo


main :: IO ()
main =
  display
    (InWindow "Polygons" (800, 600) (10, 10))
    white
    picture


picture :: Picture
picture =
  Pictures
    [ -- Row 1: Convex polygons (fast path)
      Translate (-300) 200 $ Color red convexTriangle
    , Translate (-100) 200 $ Color green convexSquare
    , Translate 100 200 $ Color blue convexPentagon
    , Translate 300 200 $ Color orange convexHexagon
    , -- Row 2: Non-convex polygons (tessellation path)
      Translate (-300) 0 $ Color violet starShape
    , Translate (-100) 0 $ Color cyan arrowShape
    , Translate 100 0 $ Color magenta lShape
    , Translate 300 0 $ Color rose crossShape
    , -- Row 3: More complex non-convex shapes
      Translate (-200) (-200) $ Color azure hourglassShape
    , Translate 0 (-200) $ Color chartreuse zigzagShape
    , Translate 200 (-200) $ Color aquamarine pacmanShape
    ]


-- Convex polygons (rendered using fast GL.Polygon path)

convexTriangle :: Picture
convexTriangle =
  Polygon
    [ (0, 50)
    , (-43, -25)
    , (43, -25)
    ]


convexSquare :: Picture
convexSquare =
  Polygon
    [ (-40, -40)
    , (40, -40)
    , (40, 40)
    , (-40, 40)
    ]


convexPentagon :: Picture
convexPentagon =
  Polygon
    [ (0, 50)
    , (-48, 15)
    , (-30, -40)
    , (30, -40)
    , (48, 15)
    ]


convexHexagon :: Picture
convexHexagon =
  Polygon
    [ (0, 50)
    , (-43, 25)
    , (-43, -25)
    , (0, -50)
    , (43, -25)
    , (43, 25)
    ]


-- Non-convex polygons (rendered using GLU tessellation)

starShape :: Picture
starShape =
  Polygon
    [ (0, 60)
    , (-15, 20)
    , (-55, 20)
    , (-25, -10)
    , (-35, -50)
    , (0, -25)
    , (35, -50)
    , (25, -10)
    , (55, 20)
    , (15, 20)
    ]


arrowShape :: Picture
arrowShape =
  Polygon
    [ (0, 50)
    , (-40, 0)
    , (-20, 0)
    , (-20, -50)
    , (20, -50)
    , (20, 0)
    , (40, 0)
    ]


lShape :: Picture
lShape =
  Polygon
    [ (-30, 50)
    , (-30, -50)
    , (30, -50)
    , (30, -20)
    , (0, -20)
    , (0, 50)
    ]


crossShape :: Picture
crossShape =
  Polygon
    [ (-15, 50)
    , (-15, 15)
    , (-50, 15)
    , (-50, -15)
    , (-15, -15)
    , (-15, -50)
    , (15, -50)
    , (15, -15)
    , (50, -15)
    , (50, 15)
    , (15, 15)
    , (15, 50)
    ]


hourglassShape :: Picture
hourglassShape =
  Polygon
    [ (-40, 50)
    , (40, 50)
    , (0, 0)
    , (40, -50)
    , (-40, -50)
    , (0, 0)
    ]


zigzagShape :: Picture
zigzagShape =
  Polygon
    [ (-50, 30)
    , (-25, 30)
    , (-25, -30)
    , (0, -30)
    , (0, 30)
    , (25, 30)
    , (25, -30)
    , (50, -30)
    , (50, -50)
    , (-50, -50)
    ]


pacmanShape :: Picture
pacmanShape =
  let
    radius = 50 :: Float
    segments = 20 :: Int
    mouthAngle = pi / 4
    angles =
      [mouthAngle, mouthAngle + step .. 2 * pi - mouthAngle]
        ++ [2 * pi - mouthAngle]
    step = (2 * pi - 2 * mouthAngle) / fromIntegral segments
  in
    Polygon $
      (0, 0) : [(radius * cos a, radius * sin a) | a <- angles]
