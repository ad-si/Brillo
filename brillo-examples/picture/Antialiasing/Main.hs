{-# LANGUAGE OverloadedStrings #-}

-- | Compare anti-aliased and non-anti-aliased versions of all graphical elements.
module Main where

import Brillo


main :: IO ()
main =
  display
    ( InWindow
        "Anti-aliasing Comparison"
        (1200, 800)
        (50, 50)
    )
    white
    picture


picture :: Picture
picture =
  Pictures
    [ -- Column headers
      Translate (-400) 350 $ Scale 0.15 0.15 $ Text "Normal"
    , Translate (-150) 350 $ Scale 0.15 0.15 $ Text "Anti-aliased"
    , -- Lines
      Translate (-500) 280 $ Scale 0.12 0.12 $ Text "Line"
    , Translate (-400) 280 $ Color blue $ line wavyPath
    , Translate (-150) 280 $ Color blue $ lineSmooth wavyPath
    , -- Thick Lines
      Translate (-500) 220 $ Scale 0.12 0.12 $ Text "ThickLine"
    , Translate (-400) 220 $ Color red $ thickLine wavyPath 3
    , Translate (-150) 220 $ Color red $ thickLineSmooth wavyPath 3
    , -- Circles
      Translate (-500) 150 $ Scale 0.12 0.12 $ Text "Circle"
    , Translate (-400) 150 $ Color green $ circle 40
    , Translate (-150) 150 $ Color green $ circleSmooth 40
    , -- Thick Circles
      Translate (-500) 70 $ Scale 0.12 0.12 $ Text "ThickCircle"
    , Translate (-400) 70 $ Color magenta $ thickCircle 30 10
    , Translate (-150) 70 $ Color magenta $ thickCircleSmooth 30 10
    , -- Solid Circles
      Translate (-500) (-10) $ Scale 0.12 0.12 $ Text "CircleSolid"
    , Translate (-400) (-10) $ Color orange $ circleSolid 35
    , Translate (-150) (-10) $ Color orange $ circleSolidSmooth 35
    , -- Arcs
      Translate (-500) (-90) $ Scale 0.12 0.12 $ Text "Arc"
    , Translate (-400) (-90) $ Color cyan $ arc 30 150 40
    , Translate (-150) (-90) $ Color cyan $ arcSmooth 30 150 40
    , -- Thick Arcs
      Translate (-500) (-160) $ Scale 0.12 0.12 $ Text "ThickArc"
    , Translate (-400) (-160) $ Color violet $ thickArc 30 150 30 10
    , Translate (-150) (-160) $ Color violet $ thickArcSmooth 30 150 30 10
    , -- Solid Arcs
      Translate (-500) (-230) $ Scale 0.12 0.12 $ Text "ArcSolid"
    , Translate (-400) (-230) $ Color rose $ arcSolid 30 150 40
    , Translate (-150) (-230) $ Color rose $ arcSolidSmooth 30 150 40
    , -- Polygons (triangles)
      Translate (-500) (-300) $ Scale 0.12 0.12 $ Text "Polygon"
    , Translate (-400) (-300) $ Color azure $ polygon trianglePath
    , Translate (-150) (-300) $ Color azure $ polygonSmooth trianglePath
    , -- Text
      Translate (-500) (-370) $ Scale 0.12 0.12 $ Text "Text"
    , Translate (-400) (-370) $ Color black $ Scale 0.15 0.15 $ text "Abc"
    , Translate (-150) (-370) $ Color black $ Scale 0.15 0.15 $ textSmooth "Abc"
    , -- Right side: Rectangles
      Translate (100) 350 $ Scale 0.15 0.15 $ Text "Normal"
    , Translate (350) 350 $ Scale 0.15 0.15 $ Text "Anti-aliased"
    , -- Rectangle Wire
      Translate (0) 280 $ Scale 0.12 0.12 $ Text "RectWire"
    , Translate (100) 280 $ Color blue $ rectangleWire 80 50
    , Translate (350) 280 $ Color blue $ rectangleWireSmooth 80 50
    , -- Rectangle Solid
      Translate (0) 200 $ Scale 0.12 0.12 $ Text "RectSolid"
    , Translate (100) 200 $ Color green $ rectangleSolid 80 50
    , Translate (350) 200 $ Color green $ rectangleSolidSmooth 80 50
    , -- Line Loop
      Translate (0) 120 $ Scale 0.12 0.12 $ Text "LineLoop"
    , Translate (100) 120 $ Color red $ lineLoop starPath
    , Translate (350) 120 $ Color red $ lineLoopSmooth starPath
    , -- Sector Wire
      Translate (0) 40 $ Scale 0.12 0.12 $ Text "SectorWire"
    , Translate (100) 40 $ Color magenta $ sectorWire 30 120 40
    , Translate (350) 40 $ Color magenta $ sectorWireSmooth 30 120 40
    , -- Thick Text
      Translate (0) (-40) $ Scale 0.12 0.12 $ Text "ThickText"
    , Translate (100) (-40) $ Color orange $ Scale 0.12 0.12 $ thickText "Xyz" 5
    , Translate (350) (-40) $ Color orange $ Scale 0.12 0.12 $ thickTextSmooth "Xyz" 5
    , -- Complex polygon (pentagon)
      Translate (0) (-120) $ Scale 0.12 0.12 $ Text "Pentagon"
    , Translate (100) (-120) $ Color cyan $ polygon pentagonPath
    , Translate (350) (-120) $ Color cyan $ polygonSmooth pentagonPath
    , -- Diagonal lines (shows aliasing clearly)
      Translate (0) (-200) $ Scale 0.12 0.12 $ Text "Diagonal"
    , Translate (100) (-200) $ Color violet $ line diagonalPath
    , Translate (350) (-200) $ Color violet $ lineSmooth diagonalPath
    , -- Small circles (shows aliasing clearly)
      Translate (0) (-280) $ Scale 0.12 0.12 $ Text "SmallCircles"
    , Translate (100) (-280) $ smallCircles circle
    , Translate (350) (-280) $ smallCircles circleSmooth
    , -- Rotated rectangle (shows aliasing on edges)
      Translate (0) (-360) $ Scale 0.12 0.12 $ Text "Rotated"
    , Translate (100) (-360) $ Color rose $ Rotate 30 $ rectangleSolid 70 40
    , Translate (350) (-360) $ Color rose $ Rotate 30 $ rectangleSolidSmooth 70 40
    ]


-- | A wavy path to show line rendering
wavyPath :: Path
wavyPath =
  [ (x, 15 * sin (x / 10))
  | x <- [-50, -45 .. 50]
  ]


-- | A triangle path
trianglePath :: Path
trianglePath = [(-30, -25), (0, 30), (30, -25)]


-- | A star-shaped path
starPath :: Path
starPath =
  [ (r * cos theta, r * sin theta)
  | i <- [0 :: Int .. 4]
  , let angle = fromIntegral i * 2 * pi / 5 - pi / 2
  , (r, theta) <- [(30, angle), (12, angle + pi / 5)]
  ]


-- | A pentagon path
pentagonPath :: Path
pentagonPath =
  [ (30 * cos theta, 30 * sin theta)
  | i <- [0 :: Int .. 4]
  , let theta = fromIntegral i * 2 * pi / 5 - pi / 2
  ]


-- | A diagonal line to clearly show aliasing artifacts
diagonalPath :: Path
diagonalPath = [(-40, -20), (40, 20)]


-- | Multiple small circles to show aliasing
smallCircles :: (Float -> Picture) -> Picture
smallCircles circFn =
  Pictures
    [ Translate (fromIntegral x * 15 - 30) 0 $
        Color (makeColor 0.2 0.4 0.8 1) $
          circFn (3 + fromIntegral x)
    | x <- [0 .. 4 :: Int]
    ]
