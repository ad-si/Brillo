{-# LANGUAGE OverloadedStrings #-}

{-| Simple GUI demo with layout and buttons using TrueType fonts.
  Demonstrates a high-level layout system with interactive buttons.
-}
module Main where

import Brillo
import Brillo.Interface.Pure.Game
import Control.Monad (foldM)
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

  let initialState =
        AppState
          { fontPath = fontPath
          , mousePos = (0, 0)
          , lastClicked = Nothing
          , hoveredButton = Nothing
          }

  play
    (InWindow "Brillo GUI Demo" (800, 600) (100, 100))
    (greyN 0.95)
    60
    initialState
    renderScene
    handleEvent
    stepWorld


-- | Application state
data AppState
  = AppState
      { fontPath :: FilePath
      , mousePos :: Point
      , lastClicked :: Maybe String
      , hoveredButton :: Maybe String
      }


-- | Button definition
data Button
  = Button
      { buttonId :: String
      , buttonLabel :: T.Text
      , buttonX :: Float
      , buttonY :: Float
      , buttonWidth :: Float
      , buttonHeight :: Float
      }


-- | Define our buttons
buttons :: [Button]
buttons =
  [ Button "new" "New" (-300) (-220) 140 50
  , Button "open" "Open" (-140) (-220) 140 50
  , Button "save" "Save" 20 (-220) 140 50
  , Button "close" "Close" 180 (-220) 140 50
  ]


-- | Check if a point is inside a button
pointInButton :: Point -> Button -> Bool
pointInButton (x, y) btn =
  let x1 = buttonX btn
      y1 = buttonY btn
      x2 = x1 + buttonWidth btn
      y2 = y1 + buttonHeight btn
  in  x >= x1 && x <= x2 && y >= y1 && y <= y2


-- | Find which button (if any) is at the given point
findButtonAt :: Point -> [Button] -> Maybe String
findButtonAt pt btns =
  case filter (pointInButton pt) btns of
    (btn : _) -> Just (buttonId btn)
    [] -> Nothing


-- | Render the entire scene
renderScene :: AppState -> Picture
renderScene state =
  pictures
    [ -- Header section
      translate 0 220 $ renderHeader (fontPath state)
    , -- Main content area
      translate 0 50 $ renderContent (fontPath state) (lastClicked state)
    , -- Button bar at bottom
      pictures $ map (renderButton state) buttons
    , -- Status bar
      translate 0 (-280) $ renderStatusBar (fontPath state) (mousePos state)
    ]


-- | Render the header with title
renderHeader :: FilePath -> Picture
renderHeader font =
  pictures
    [ -- Header background
      color (makeColor 0.2 0.4 0.7 1.0) $ rectangleSolid 800 80
    , -- Title text
      translate (-350) (-10) $
        color white $
          truetypeText font 42 "Brillo GUI Demo"
    ]


-- | Render the main content area
renderContent :: FilePath -> Maybe String -> Picture
renderContent font mLastClicked =
  pictures
    [ -- Content background
      color (makeColor 0.98 0.98 1.0 1.0) $ rectangleSolid 760 300
    , -- Border
      color (greyN 0.7) $ rectangleWire 760 300
    , -- Display last clicked button
      case mLastClicked of
        Nothing ->
          translate (-250) 40 $
            color (greyN 0.5) $
              truetypeText font 32 "Click a button below!"
        Just btnId ->
          pictures
            [ translate (-300) 60 $
                color (makeColor 0.2 0.6 0.3 1.0) $
                  truetypeText font 32 "Button clicked:"
            , translate (-300) 0 $
                color (makeColor 0.3 0.3 0.8 1.0) $
                  truetypeText font 48 (T.pack btnId)
            , translate (-300) (-70) $
                color (greyN 0.4) $
                  truetypeText font 24 "Try clicking other buttons!"
            ]
    ]


-- | Render a single button
renderButton :: AppState -> Button -> Picture
renderButton state btn =
  let isHovered = hoveredButton state == Just (buttonId btn)
      isClicked = lastClicked state == Just (buttonId btn)

      -- Choose colors based on state
      bgColor
        | isClicked = makeColor 0.2 0.7 0.3 1.0
        | isHovered = makeColor 0.4 0.6 0.9 1.0
        | otherwise = makeColor 0.5 0.5 0.5 1.0

      textColor
        | isClicked || isHovered = white
        | otherwise = greyN 0.95

      -- Calculate center position for the button
      cx = buttonX btn + buttonWidth btn / 2
      cy = buttonY btn + buttonHeight btn / 2

      -- Text positioning (centered in button)
      textOffset = -12 -- Approximate vertical centering for text
  in  translate cx cy $
        pictures
          [ -- Button background
            color bgColor $
              rectangleSolid (buttonWidth btn) (buttonHeight btn)
          , -- Button border
            color (greyN 0.2) $
              rectangleWire (buttonWidth btn) (buttonHeight btn)
          , -- Button label
            translate (-50) textOffset $
              color textColor $
                truetypeText (fontPath state) 28 (buttonLabel btn)
          ]


-- | Render the status bar showing mouse position
renderStatusBar :: FilePath -> Point -> Picture
renderStatusBar font (mx, my) =
  pictures
    [ color (greyN 0.85) $ rectangleSolid 800 40
    , translate (-380) (-8) $
        color (greyN 0.3) $
          truetypeText
            font
            18
            (T.pack $ "Mouse: (" ++ show (round mx :: Int) ++ ", " ++ show (round my :: Int) ++ ")")
    ]


-- | Handle input events
handleEvent :: Event -> AppState -> AppState
handleEvent event state =
  case event of
    -- Track mouse position
    EventMotion pos ->
      state
        { mousePos = pos
        , hoveredButton = findButtonAt pos buttons
        }
    -- Handle mouse clicks on buttons
    EventKey (MouseButton LeftButton) Down _ pos ->
      case findButtonAt pos buttons of
        Just btnId -> state {lastClicked = Just btnId}
        Nothing -> state
    -- Other events
    _ -> state


-- | Update function (not used in this static demo)
stepWorld :: Float -> AppState -> AppState
stepWorld _ = id


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
          [ "brillo-gui: unable to locate a TrueType font."
          , "Tried the following paths:"
          , unlines (map ("  • " <>) candidateFonts)
          , "Pass a font explicitly: stack run brillo-gui -- /path/to/font.ttf"
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
      hPutStrLn stderr $ "brillo-gui: font file not found: " <> fp
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
