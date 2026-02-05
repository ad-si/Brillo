{-# LANGUAGE OverloadedStrings #-}

{-| Cursor demo showing all available cursor shapes.
  Hover over each button to see the corresponding cursor.
-}
module Main where

import Brillo
import Brillo.Interface.IO.Interact
import Control.Monad (foldM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


main :: IO ()
main = do
  args <- getArgs
  let fontArg = listToMaybe [a | a <- args, not ("--" `isPrefixOf` a)]
  fontPath <- resolveFont fontArg

  -- Create an IORef to store the controller once we get it
  controllerRef <- newIORef Nothing

  let initialState =
        AppState
          { fontPath = fontPath
          , mousePos = (0, 0)
          , hoveredCursor = Nothing
          }

  interactIO
    (InWindow "Cursor Demo" (600, 500) (100, 100))
    (greyN 0.95)
    initialState
    (pure . renderScene)
    (handleEvent controllerRef)
    (writeIORef controllerRef . Just)


-- | Application state
data AppState
  = AppState
  { fontPath :: FilePath
  , mousePos :: Point
  , hoveredCursor :: Maybe CursorShape
  }


-- | Button definition linking to a cursor shape
data CursorButton
  = CursorButton
  { cbShape :: CursorShape
  , cbLabel :: T.Text
  , cbDescription :: T.Text
  , cbX :: Float
  , cbY :: Float
  , cbWidth :: Float
  , cbHeight :: Float
  }


-- | Define buttons for each cursor type
cursorButtons :: [CursorButton]
cursorButtons =
  [ CursorButton CursorArrow "Arrow" "Default pointer" (-200) 80 180 60
  , CursorButton CursorHand "Hand" "Clickable items" (50) 80 180 60
  , CursorButton CursorIBeam "I-Beam" "Text selection" (-200) 0 180 60
  , CursorButton CursorCrosshair "Crosshair" "Precision select" (50) 0 180 60
  , CursorButton CursorResizeH "Resize H" "Horizontal resize" (-200) (-80) 180 60
  , CursorButton CursorResizeV "Resize V" "Vertical resize" (50) (-80) 180 60
  , CursorButton CursorHidden "Hidden" "No cursor" (-75) (-160) 180 60
  ]


-- | Check if a point is inside a button
pointInButton :: Point -> CursorButton -> Bool
pointInButton (x, y) btn =
  let x1 = cbX btn
      y1 = cbY btn
      x2 = x1 + cbWidth btn
      y2 = y1 + cbHeight btn
  in  x >= x1 && x <= x2 && y >= y1 && y <= y2


-- | Find which cursor button (if any) is at the given point
findButtonAt :: Point -> [CursorButton] -> Maybe CursorButton
findButtonAt pt btns =
  case filter (pointInButton pt) btns of
    (btn : _) -> Just btn
    [] -> Nothing


-- | Render the entire scene
renderScene :: AppState -> Picture
renderScene state =
  pictures
    [ -- Header
      translate 0 200 $ renderHeader (fontPath state)
    , -- Buttons
      pictures $ map (renderButton state) cursorButtons
    , -- Status bar
      translate 0 (-200) $ renderStatusBar state
    ]


-- | Render the header
renderHeader :: FilePath -> Picture
renderHeader font =
  pictures
    [ color (makeColor 0.2 0.5 0.7 1.0) $ rectangleSolid 600 60
    , translate (-250) (-12) $
        color white $
          truetypeText font 32 "Cursor Shape Demo"
    ]


-- | Render a cursor button
renderButton :: AppState -> CursorButton -> Picture
renderButton state btn =
  let isHovered = hoveredCursor state == Just (cbShape btn)

      bgColor
        | isHovered = makeColor 0.3 0.6 0.9 1.0
        | otherwise = makeColor 0.6 0.6 0.6 1.0

      textColor
        | isHovered = white
        | otherwise = greyN 0.95

      cx = cbX btn + cbWidth btn / 2
      cy = cbY btn + cbHeight btn / 2
  in  translate cx cy $
        pictures
          [ -- Background
            color bgColor $ rectangleSolid (cbWidth btn) (cbHeight btn)
          , -- Border
            color (greyN 0.3) $ rectangleWire (cbWidth btn) (cbHeight btn)
          , -- Label
            translate (-80) 5 $
              color textColor $
                truetypeText (fontPath state) 22 (cbLabel btn)
          , -- Description
            translate (-80) (-18) $
              color (if isHovered then greyN 0.9 else greyN 0.75) $
                truetypeText (fontPath state) 14 (cbDescription btn)
          ]


-- | Render the status bar
renderStatusBar :: AppState -> Picture
renderStatusBar state =
  let cursorText = case hoveredCursor state of
        Nothing -> "Hover over a button to change cursor"
        Just shape -> "Current cursor: " <> T.pack (show shape)
  in  pictures
        [ color (greyN 0.85) $ rectangleSolid 600 50
        , translate (-270) (-8) $
            color (greyN 0.3) $
              truetypeText (fontPath state) 18 cursorText
        ]


-- | Handle input events
handleEvent :: IORef (Maybe Controller) -> Event -> AppState -> IO AppState
handleEvent controllerRef event state =
  case event of
    EventMotion pos -> do
      let mBtn = findButtonAt pos cursorButtons
          newHovered = cbShape <$> mBtn

      -- Update cursor when hovering changes
      mCtrl <- readIORef controllerRef
      case (mCtrl, newHovered) of
        (Just ctrl, Just shape) -> controllerSetCursor ctrl shape
        (Just ctrl, Nothing) -> controllerSetCursor ctrl CursorArrow
        _ -> pure ()

      pure $
        state
          { mousePos = pos
          , hoveredCursor = newHovered
          }
    _ -> pure state


-- | Font resolution logic
resolveFont :: Maybe FilePath -> IO FilePath
resolveFont (Just fp) = ensureFont fp
resolveFont Nothing = do
  found <- foldM pick Nothing candidateFonts
  case found of
    Just fp -> pure fp
    Nothing -> do
      hPutStrLn stderr $
        unlines
          [ "brillo-cursors: unable to locate a TrueType font."
          , "Tried the following paths:"
          , unlines (map ("  - " <>) candidateFonts)
          , "Pass a font explicitly: stack run brillo-cursors -- /path/to/font.ttf"
          ]
      exitFailure
  where
    pick acc candidate =
      case acc of
        Just _ -> pure acc
        Nothing -> do
          exists <- doesFileExist candidate
          pure $ if exists then Just candidate else Nothing


ensureFont :: FilePath -> IO FilePath
ensureFont fp = do
  exists <- doesFileExist fp
  if exists
    then pure fp
    else do
      hPutStrLn stderr $ "brillo-cursors: font file not found: " <> fp
      exitFailure


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
