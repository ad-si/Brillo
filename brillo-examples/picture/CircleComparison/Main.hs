{-# LANGUAGE OverloadedStrings #-}

{-| Comprehensive comparison of anti-aliased vs aliased circle rendering.

This example tests various edge cases to identify rendering artifacts:
- Very small circles (sub-pixel to a few pixels)
- Very large circles
- Circles at different zoom levels
- Various thickness values
- Overlapping/adjacent circles
- Circles with different colors and alpha
- Rotated/translated circles
- Arcs with various angle ranges
-}
module Main where

import Brillo
import Brillo.Interface.Pure.Game
import Data.Text qualified as T


main :: IO ()
main =
  play
    ( InWindow
        "Circle Comparison - Anti-aliased vs Aliased"
        (1400, 900)
        (50, 50)
    )
    white
    60
    initialWorld
    drawWorld
    handleEvent
    stepWorld


-- | World state with zoom and pan
data World = World
  { worldZoom :: !Float
  , worldPanX :: !Float
  , worldPanY :: !Float
  }


initialWorld :: World
initialWorld = World 1.0 0 0


stepWorld :: Float -> World -> World
stepWorld _ = id


handleEvent :: Event -> World -> World
handleEvent event world = case event of
  -- Zoom with scroll wheel or +/-
  EventKey (SpecialKey KeyUp) Down _ _ ->
    world{worldZoom = worldZoom world * 1.2}
  EventKey (SpecialKey KeyDown) Down _ _ ->
    world{worldZoom = worldZoom world / 1.2}
  EventKey (Char '+') Down _ _ ->
    world{worldZoom = worldZoom world * 1.2}
  EventKey (Char '-') Down _ _ ->
    world{worldZoom = worldZoom world / 1.2}
  EventKey (Char '=') Down _ _ ->
    world{worldZoom = worldZoom world * 1.2}
  -- Pan with arrow keys
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    world{worldPanX = worldPanX world + 50}
  EventKey (SpecialKey KeyRight) Down _ _ ->
    world{worldPanX = worldPanX world - 50}
  -- Reset with 'r'
  EventKey (Char 'r') Down _ _ -> initialWorld
  _ -> world


drawWorld :: World -> Picture
drawWorld world =
  Pictures
    [ -- Instructions at top
      Translate (-680) 420 $
        Scale 0.1 0.1 $
          Text "Up/Down or +/-: Zoom | Left/Right: Pan | R: Reset"
    , Translate (-680) 390 $
        Scale 0.1 0.1 $
          Text $
            T.pack $
              "Zoom: " ++ show (worldZoom world)
    , -- Apply zoom and pan to test content
      Translate (worldPanX world) (worldPanY world) $
        Scale (worldZoom world) (worldZoom world) $
          testContent
    ]


testContent :: Picture
testContent =
  Pictures
    [ -- Section 1: Size comparison (very small to large)
      sizeComparisonSection
    , -- Section 2: Thickness variations
      thicknessSection
    , -- Section 3: Arc angle variations
      arcSection
    , -- Section 4: Adjacent/overlapping circles
      adjacentSection
    , -- Section 5: Alpha/transparency
      alphaSection
    , -- Section 6: Stress test grid
      gridSection
    ]


-- | Compare circles of various sizes
sizeComparisonSection :: Picture
sizeComparisonSection =
  Translate (-550) 280 $
    Pictures
      [ -- Header
        Translate 0 70 $ Scale 0.12 0.12 $ Text "Size Comparison"
      , Translate 0 50 $ Scale 0.08 0.08 $ Text "Anti-aliased (top) vs Aliased (bottom)"
      , -- Row of circles with increasing sizes
        Pictures
          [ let xPos = fromIntegral i * 70
                radius = sizes !! i
            in  Pictures
                  [ -- Anti-aliased
                    Translate xPos 20 $ Color blue $ circleSolid radius
                  , -- Aliased
                    Translate xPos (-30) $ Color blue $ circleSolidAliased radius
                  , -- Size label
                    Translate xPos (-60) $
                      Scale 0.06 0.06 $
                        Text (T.pack $ show radius ++ "px")
                  ]
          | i <- [0 .. length sizes - 1]
          ]
      ]
  where
    sizes = [0.5, 1, 2, 3, 5, 8, 15, 25, 40]


-- | Compare circles with various thicknesses
thicknessSection :: Picture
thicknessSection =
  Translate (-550) 130 $
    Pictures
      [ -- Header
        Translate 0 70 $ Scale 0.12 0.12 $ Text "Thickness Variations"
      , -- Row of thick circles
        Pictures
          [ let xPos = fromIntegral i * 90
                thickness = thicknesses !! i
            in  Pictures
                  [ -- Anti-aliased
                    Translate xPos 20 $ Color red $ ThickCircle 25 thickness
                  , -- Aliased
                    Translate xPos (-40) $ Color red $ ThickCircleAliased 25 thickness
                  , -- Thickness label
                    Translate xPos (-80) $
                      Scale 0.06 0.06 $
                        Text (T.pack $ "t=" ++ show thickness)
                  ]
          | i <- [0 .. length thicknesses - 1]
          ]
      ]
  where
    thicknesses = [1, 2, 5, 10, 20, 30, 50]


-- | Compare arcs with various angle ranges
arcSection :: Picture
arcSection =
  Translate (-550) (-40) $
    Pictures
      [ -- Header
        Translate 0 70 $ Scale 0.12 0.12 $ Text "Arc Angle Variations"
      , -- Row of arcs
        Pictures
          [ let xPos = fromIntegral i * 100
                (a1, a2) = angles !! i
            in  Pictures
                  [ -- Anti-aliased
                    Translate xPos 15 $ Color green $ arcSolid a1 a2 30
                  , -- Aliased
                    Translate xPos (-50) $ Color green $ arcSolidAliased a1 a2 30
                  , -- Angle label
                    Translate xPos (-90) $
                      Scale 0.05 0.05 $
                        Text (T.pack $ show (round a1 :: Int) ++ "-" ++ show (round a2 :: Int))
                  ]
          | i <- [0 .. length angles - 1]
          ]
      ]
  where
    angles =
      [ (0, 30) -- Small arc
      , (0, 90) -- Quarter
      , (0, 180) -- Half
      , (0, 270) -- Three-quarter
      , (0, 359) -- Almost full
      , (45, 135) -- Arbitrary
      , (-45, 45) -- Crossing 0
      ]


-- | Test adjacent and overlapping circles
adjacentSection :: Picture
adjacentSection =
  Translate (200) 280 $
    Pictures
      [ -- Header
        Translate 0 70 $ Scale 0.12 0.12 $ Text "Adjacent/Overlapping"
      , -- Adjacent circles (normal)
        Translate (-80) 0 $
          Pictures
            [ Translate (-20) 0 $ Color orange $ circleSolid 20
            , Translate 20 0 $ Color orange $ circleSolid 20
            ]
      , -- Adjacent circles (smooth)
        Translate (80) 0 $
          Pictures
            [ Translate (-20) 0 $ Color orange $ circleSolidAliased 20
            , Translate 20 0 $ Color orange $ circleSolidAliased 20
            ]
      , -- Labels
        Translate (-80) (-40) $ Scale 0.06 0.06 $ Text "Anti-aliased"
      , Translate 80 (-40) $ Scale 0.06 0.06 $ Text "Aliased"
      , -- Overlapping circles (normal)
        Translate (-80) (-80) $
          Pictures
            [ Translate (-10) 0 $ Color (withAlpha 0.7 cyan) $ circleSolid 20
            , Translate 10 0 $ Color (withAlpha 0.7 magenta) $ circleSolid 20
            ]
      , -- Overlapping circles (smooth)
        Translate (80) (-80) $
          Pictures
            [ Translate (-10) 0 $ Color (withAlpha 0.7 cyan) $ circleSolidAliased 20
            , Translate 10 0 $ Color (withAlpha 0.7 magenta) $ circleSolidAliased 20
            ]
      ]


-- | Test alpha/transparency rendering
alphaSection :: Picture
alphaSection =
  Translate (200) 100 $
    Pictures
      [ -- Header
        Translate 0 70 $ Scale 0.12 0.12 $ Text "Alpha Transparency"
      , -- Background rectangle
        Color (greyN 0.8) $ rectangleSolid 200 100
      , -- Anti-aliased with various alphas
        Translate (-60) 0 $
          Pictures
            [ Translate 0 20 $ Color (withAlpha 1.0 red) $ circleSolid 15
            , Translate 0 0 $ Color (withAlpha 0.7 red) $ circleSolid 15
            , Translate 0 (-20) $ Color (withAlpha 0.3 red) $ circleSolid 15
            ]
      , -- Aliased with various alphas
        Translate 60 0 $
          Pictures
            [ Translate 0 20 $ Color (withAlpha 1.0 red) $ circleSolidAliased 15
            , Translate 0 0 $ Color (withAlpha 0.7 red) $ circleSolidAliased 15
            , Translate 0 (-20) $ Color (withAlpha 0.3 red) $ circleSolidAliased 15
            ]
      , -- Labels
        Translate (-60) (-55) $ Scale 0.06 0.06 $ Text "Anti-aliased"
      , Translate 60 (-55) $ Scale 0.06 0.06 $ Text "Aliased"
      ]


-- | Grid of small circles to stress test at various zoom levels
gridSection :: Picture
gridSection =
  Translate (450) (-100) $
    Pictures
      [ -- Header
        Translate 0 140 $ Scale 0.12 0.12 $ Text "Stress Test Grid"
      , Translate 0 120 $ Scale 0.07 0.07 $ Text "Zoom in/out to test edge cases"
      , -- Anti-aliased grid
        Translate (-100) 0 $
          Pictures
            [ Translate 0 (-120) $ Scale 0.06 0.06 $ Text "Anti-aliased"
            , circleGrid circleSolid
            ]
      , -- Aliased grid
        Translate 100 0 $
          Pictures
            [ Translate 0 (-120) $ Scale 0.06 0.06 $ Text "Aliased"
            , circleGrid circleSolidAliased
            ]
      ]


-- | Create a grid of small circles
circleGrid :: (Float -> Picture) -> Picture
circleGrid circleFn =
  Pictures
    [ Translate (fromIntegral x * spacing) (fromIntegral y * spacing) $
        Color (makeColor r g b 1) $
          circleFn radius
    | x <- [-gridSize .. gridSize]
    , y <- [-gridSize .. gridSize]
    , let radius = 3 + fromIntegral (abs x + abs y) * 0.3
          r = 0.3 + 0.1 * fromIntegral x / fromIntegral gridSize
          g = 0.3 + 0.1 * fromIntegral y / fromIntegral gridSize
          b = 0.8
    ]
  where
    gridSize = 5 :: Int
    spacing = 18 :: Float
