{-# LANGUAGE OverloadedStrings #-}

{-| Interactive canvas editor with draggable shapes and SVG export.

  This example demonstrates:
  - Creating graphical elements (shapes) on a canvas
  - Dragging shapes with the mouse
  - A clickable "Export as SVG" button
  - Using the file save dialog to export the canvas
-}
module Main where

import Brillo
import Brillo.Export.SVG (exportPictureToSVG)
import Brillo.Interface.Environment (saveFileDialog)
import Brillo.Interface.IO.Game
import Control.Monad (foldM)
import Data.Maybe (listToMaybe)
import System.Directory (doesFileExist)
import System.Environment (getArgs)


-- | A shape on the canvas
data Shape = Shape
  { shapeType :: ShapeType
  , shapePos :: Point
  , shapeColor :: Color
  }


data ShapeType
  = -- | Circle with radius
    CircleShape Float
  | -- | Rectangle with width and height
    RectShape Float Float
  | -- | Equilateral triangle with side length
    TriangleShape Float


-- | Application state
data AppState = AppState
  { shapes :: [Shape]
  -- ^ All shapes on the canvas
  , dragging :: Maybe Int
  -- ^ Index of shape being dragged (if any)
  , dragOffset :: Point
  -- ^ Offset from shape center to mouse when drag started
  , mousePos :: Point
  -- ^ Current mouse position
  , fontPath :: FilePath
  -- ^ Path to TrueType font
  }


-- | Layout dimensions
headerHeight :: Float
headerHeight = 50


canvasWidth, canvasHeight :: Int
canvasWidth = 800
canvasHeight = 450


buttonHeight :: Float
buttonHeight = 50


-- Total window height: header + canvas + button area
windowWidth, windowHeight :: Int
windowWidth = canvasWidth
windowHeight = round headerHeight + canvasHeight + round buttonHeight + 20


-- Y positions (origin at center of window)
headerY :: Float
headerY = fromIntegral windowHeight / 2 - headerHeight / 2


canvasY :: Float
canvasY = headerY - headerHeight / 2 - fromIntegral canvasHeight / 2


buttonY :: Float
buttonY = canvasY - fromIntegral canvasHeight / 2 - buttonHeight / 2 - 10


-- | Initial shapes on the canvas (positions relative to canvas center)
initialShapes :: [Shape]
initialShapes =
  [ Shape (CircleShape 50) (-150, 100) red
  , Shape (RectShape 80 60) (100, 50) blue
  , Shape (TriangleShape 70) (0, -100) green
  , Shape (CircleShape 30) (200, 150) orange
  , Shape (RectShape 50 80) (-200, -50) violet
  ]


-- | Initial application state
initialState :: FilePath -> AppState
initialState font =
  AppState
    { shapes = initialShapes
    , dragging = Nothing
    , dragOffset = (0, 0)
    , mousePos = (0, 0)
    , fontPath = font
    }


-- | Convert window coordinates to canvas-local coordinates
windowToCanvas :: Point -> Point
windowToCanvas (wx, wy) = (wx, wy - canvasY)


-- | Convert canvas-local coordinates to window coordinates
canvasToWindow :: Point -> Point
canvasToWindow (cx, cy) = (cx, cy + canvasY)


-- | Check if a point (in window coords) is inside the canvas
pointInCanvas :: Point -> Bool
pointInCanvas (px, py) =
  let halfW = fromIntegral canvasWidth / 2
      halfH = fromIntegral canvasHeight / 2
      cy = py - canvasY
  in  px >= -halfW
        && px <= halfW
        && cy >= -halfH
        && cy <= halfH


-- | Check if a point (in canvas-local coords) is inside a shape
pointInShape :: Point -> Shape -> Bool
pointInShape (px, py) (Shape shapeT (sx, sy) _) =
  case shapeT of
    CircleShape r ->
      let dx = px - sx
          dy = py - sy
      in  dx * dx + dy * dy <= r * r
    RectShape w h ->
      let halfW = w / 2
          halfH = h / 2
      in  px >= sx - halfW
            && px <= sx + halfW
            && py >= sy - halfH
            && py <= sy + halfH
    TriangleShape side ->
      -- Approximate with bounding circle
      let r = side * 0.6
          dx = px - sx
          dy = py - sy
      in  dx * dx + dy * dy <= r * r


-- | Check if a point (in window coords) is inside the export button
pointInButton :: Point -> Bool
pointInButton (px, py) =
  let btnWidth = 180
      halfW = btnWidth / 2
      halfH = buttonHeight / 2
  in  px >= -halfW
        && px <= halfW
        && py >= buttonY - halfH
        && py <= buttonY + halfH


-- | Find the topmost shape at a given point (last in list = topmost)
findShapeAt :: Point -> [Shape] -> Maybe Int
findShapeAt pt shps =
  case [i | (i, s) <- zip [0 ..] shps, pointInShape pt s] of
    [] -> Nothing
    is -> Just (last is)


-- | Render a single shape
renderShape :: Shape -> Picture
renderShape (Shape shapeT (x, y) c) =
  Translate x y $
    Color c $
      case shapeT of
        CircleShape r -> circleSolid r
        RectShape w h -> rectangleSolid w h
        TriangleShape side ->
          let h = side * sqrt 3 / 2
              pts =
                [ (0, h * 2 / 3)
                , (-side / 2, -h / 3)
                , (side / 2, -h / 3)
                ]
          in  Polygon pts


-- | Render the header with instructions
renderHeader :: FilePath -> Picture
renderHeader font =
  Translate 0 headerY $
    Pictures
      [ -- Header background (dark gray like the window background)
        Color (greyN 0.8) $
          rectangleSolid (fromIntegral canvasWidth) headerHeight
      , -- Instructions text
        Translate (-380) (-8) $
          Color (greyN 0.3) $
            truetypeText
              font
              20
              "Drag shapes to move them. Click the button below to export as SVG."
      ]


-- | Render the export button
renderButton :: FilePath -> Bool -> Picture
renderButton font isHovered =
  Translate 0 buttonY $
    Pictures
      [ -- Button background
        Color
          (if isHovered then makeColor 0.3 0.6 0.9 1.0 else makeColor 0.2 0.5 0.8 1.0) $
          rectangleSolid 180 buttonHeight
      , -- Button border
        Color (greyN 0.2) $ rectangleWire 180 buttonHeight
      , -- Button text
        Translate (-70) (-8) $
          Color white $
            truetypeText font 22 "Export as SVG"
      ]


-- | Render the canvas background
renderCanvas :: Picture
renderCanvas =
  Translate 0 canvasY $
    Pictures
      [ Color (greyN 0.95) $
          rectangleSolid (fromIntegral canvasWidth) (fromIntegral canvasHeight)
      , Color (greyN 0.7) $
          rectangleWire (fromIntegral canvasWidth) (fromIntegral canvasHeight)
      ]


-- | Render the canvas content (shapes only, for export)
renderCanvasContent :: AppState -> Picture
renderCanvasContent state =
  Pictures $ map renderShape (shapes state)


-- | Render the entire scene
renderScene :: AppState -> IO Picture
renderScene state = do
  let isHovered = pointInButton (mousePos state)
  pure $
    Pictures
      [ renderHeader (fontPath state)
      , renderCanvas
      , Translate 0 canvasY $ renderCanvasContent state
      , renderButton (fontPath state) isHovered
      ]


-- | Handle input events
handleEvent :: Event -> AppState -> IO AppState
handleEvent event state =
  case event of
    -- Track mouse position
    EventMotion pos ->
      case dragging state of
        -- If dragging, update the shape position (in canvas-local coords)
        Just idx ->
          let (ox, oy) = dragOffset state
              canvasPos = windowToCanvas pos
              (mx, my) = canvasPos
              newPos = (mx - ox, my - oy)
              shps = shapes state
              updatedShapes = updateAt idx (\s -> s{shapePos = newPos}) shps
          in  pure state{shapes = updatedShapes, mousePos = pos}
        -- Otherwise just track mouse position
        Nothing ->
          pure state{mousePos = pos}
    -- Mouse button down
    EventKey (MouseButton LeftButton) Down _ pos -> do
      -- Check if clicking the export button
      if pointInButton pos
        then do
          exportCanvas state
          pure state{mousePos = pos}
        -- Check if clicking in the canvas
        else
          if pointInCanvas pos
            then do
              let canvasPos = windowToCanvas pos
              case findShapeAt canvasPos (shapes state) of
                Just idx ->
                  let (sx, sy) = shapePos (shapes state !! idx)
                      (mx, my) = canvasPos
                      offset = (mx - sx, my - sy)
                  in  pure state{dragging = Just idx, dragOffset = offset, mousePos = pos}
                Nothing ->
                  pure state{mousePos = pos}
            else
              pure state{mousePos = pos}

    -- Mouse button up - stop dragging
    EventKey (MouseButton LeftButton) Up _ pos ->
      pure state{dragging = Nothing, mousePos = pos}
    -- Ignore other events
    _ -> pure state


-- | Update an element at a specific index in a list
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs =
  [if i == idx then f x else x | (i, x) <- zip [0 ..] xs]


-- | Export the canvas to SVG
exportCanvas :: AppState -> IO ()
exportCanvas state = do
  result <-
    saveFileDialog
      "Export as SVG"
      "canvas.svg"
      ["*.svg"]
      "SVG files"
  case result of
    Nothing -> pure () -- User cancelled
    Just filePath -> do
      let canvasPicture = renderCanvasContent state
          size = (canvasWidth, canvasHeight)
          bgColor = greyN 0.95
      exportPictureToSVG size bgColor filePath canvasPicture
      putStrLn $ "Exported to: " ++ filePath


-- | Step function (not used, but required by playIO)
stepWorld :: Float -> AppState -> IO AppState
stepWorld _ state = pure state


-- | Main entry point
main :: IO ()
main = do
  putStrLn "Canvas Editor - Drag shapes and export as SVG"
  putStrLn ""

  args <- getArgs
  let fontArg = listToMaybe args
  font <- resolveFont fontArg

  playIO
    (InWindow "Canvas Editor" (windowWidth, windowHeight) (100, 100))
    (greyN 0.8)
    60 -- 60 FPS
    (initialState font)
    renderScene
    handleEvent
    stepWorld


-- | Font resolution logic
resolveFont :: Maybe FilePath -> IO FilePath
resolveFont (Just fp) = pure fp
resolveFont Nothing = do
  found <- foldM pick Nothing candidateFonts
  case found of
    Just fp -> pure fp
    Nothing -> do
      putStrLn
        "Warning: Could not find a TrueType font. Text may not render correctly."
      putStrLn "You can specify a font path as a command line argument."
      pure ""
  where
    pick acc candidate =
      case acc of
        Just _ -> pure acc
        Nothing -> do
          exists <- doesFileExist candidate
          pure $ if exists then Just candidate else Nothing


candidateFonts :: [FilePath]
candidateFonts =
  [ "/System/Library/Fonts/Supplemental/Arial.ttf"
  , "/Library/Fonts/Arial.ttf"
  , "/System/Library/Fonts/Supplemental/Helvetica.ttc"
  , "/System/Library/Fonts/Supplemental/Tahoma.ttf"
  , "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
  , "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
  , "/usr/share/fonts/truetype/freefont/FreeSans.ttf"
  , "C:\\Windows\\Fonts\\arial.ttf"
  , "C:\\Windows\\Fonts\\segoeui.ttf"
  ]
