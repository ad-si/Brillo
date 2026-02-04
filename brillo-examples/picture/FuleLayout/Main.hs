{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| FULE Layout Engine Demo

This example demonstrates the key features of FULE (Functional UI Layout Engine):

1. **Grid layouts** - Evenly spaced 2D grids of items
2. **Layered containers** - Overlapping elements in the same bounds
3. **Padded containers** - Content with configurable margins
4. **Sized containers** - Fixed-size elements
5. **Positioned containers** - Place content at corners, edges, or center
6. **Dynamic resizing** - Layouts that respond to window size changes
6. **Flat data structures** - Easy-to-traverse component info

The demo shows a dashboard-style layout with:
- A grid of colored panels
- A status indicator overlay
-}
module Main where

import Brillo
import Brillo.Interface.Pure.Game
import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.Text qualified as T
import FULE


-- | Widget types for our UI
data Widget
  = -- | Controls window resize, storing guide IDs for dynamic layout updates
    WindowResize
      { widgetWidthGuide :: GuideID
      , widgetHeightGuide :: GuideID
      }
  | -- | A colored panel with a label
    Panel
      { panelLabel :: T.Text
      , panelColor :: Color
      }
  | -- | Overlay indicator (layered on top)
    StatusIndicator T.Text


-- | Application state
data AppState = AppState
  { stateLayout :: Layout
  , stateComponents :: [ComponentInfo Widget]
  , stateWindowSize :: (Int, Int)
  , stateHoveredPanel :: Maybe T.Text
  , stateSelectedPanel :: Maybe T.Text
  }


-- | Initial window dimensions
initialWidth, initialHeight :: Int
initialWidth = 800
initialHeight = 600


-- | Build the complete layout demonstrating FULE's features
buildLayout :: Int -> Int -> (Layout, [ComponentInfo Widget])
buildLayout width height =
  layout
    ( window
        (width, height)
        WindowResize -- WindowAdjustorGen creates the resize controller
        mainContainer
    )
  where
    -- Feature: Layered container for overlapping elements
    -- This lets us place a status indicator on top of the main grid
    mainContainer :: Layered Widget
    mainContainer =
      layered
        ( [ item contentGrid
          , item statusOverlay
          ] ::
            [Item Widget]
        )

    -- Feature: Padded container wraps the grid with margins
    -- Feature: Grid layout for evenly-spaced content panels
    contentGrid :: Padded (Grid Widget)
    contentGrid =
      padded
        (padding 20 20) -- horizontal, vertical padding
        -- Feature: 2D Grid layout - items fill from top-left, row by row
        ( grid
            (3, 3) -- 3 columns, 3 rows
            ( [ item (Panel "Analytics" (makeColorI 66 133 244 255)) -- Blue
              , item (Panel "Users" (makeColorI 52 168 83 255)) -- Green
              , item (Panel "Revenue" (makeColorI 251 188 5 255)) -- Yellow
              , item (Panel "Traffic" (makeColorI 234 67 53 255)) -- Red
              , item (Panel "Events" (makeColorI 155 89 182 255)) -- Purple
              , item (Panel "Reports" (makeColorI 26 188 156 255)) -- Teal
              , item (Panel "Settings" (makeColorI 241 196 15 255)) -- Gold
              , item (Panel "Logs" (makeColorI 52 73 94 255)) -- Dark blue-grey
              , item (Panel "Help" (makeColorI 149 165 166 255)) -- Grey
              ] ::
                [Item Widget]
            )
        )

    -- Feature: Positioned + Sized containers
    -- Positioned places content at one of 9 positions (corners, edges, center)
    -- Sized gives it fixed dimensions
    statusOverlay :: Positioned (Padded (Sized (ItemM Identity Widget)))
    statusOverlay =
      topRight $ -- Position in top-right corner
        padded (padding 10 10) $ -- Add some margin from the edge
          sized
            (80, 25)
            (item (StatusIndicator "Live"))


-- | Create initial application state
initialState :: AppState
initialState =
  let (ly, comps) = buildLayout initialWidth initialHeight
  in  AppState
        { stateLayout = ly
        , stateComponents = comps
        , stateWindowSize = (initialWidth, initialHeight)
        , stateHoveredPanel = Nothing
        , stateSelectedPanel = Nothing
        }


-- | Convert layout to Brillo Picture
renderState :: AppState -> Picture
renderState state =
  Pictures $ map renderComponent (stateComponents state)
  where
    hovered = stateHoveredPanel state
    selected = stateSelectedPanel state

    renderComponent :: ComponentInfo Widget -> Picture
    renderComponent compInfo =
      case componentOf compInfo of
        WindowResize{} -> Blank -- Invisible resize controller
        Panel label col ->
          let bounds = boundsOf compInfo
              (x, y, w, h) = boundsToRect bounds
              isHovered = hovered == Just label
              isSelected = selected == Just label
              bgColor
                | isSelected = brighten (brighten col)
                | isHovered = brighten col
                | otherwise = col
              borderColor
                | isSelected = white
                | otherwise = greyN 0.2
          in  translateToCenter x y w h $
                Pictures $
                  [Color bgColor $ rectangleSolid w h]
                    ++ if isSelected
                      then [Color borderColor $ rectangleWire (w - 4) (h - 4)]
                      else [Color borderColor $ rectangleWire w h]
        StatusIndicator _ ->
          let bounds = boundsOf compInfo
              (x, y, w, h) = boundsToRect bounds
          in  translateToCenter x y w h $
                Pictures
                  [ Color (makeColorI 52 168 83 220) $ rectangleSolid w h
                  , Color (greyN 0.1) $ rectangleWire w h
                  ]

    -- Convert FULE bounds to rectangle dimensions
    boundsToRect :: Bounds -> (Float, Float, Float, Float)
    boundsToRect bounds =
      let ly = stateLayout state
          top = fromIntegral $ getGuide (topOf bounds) ly
          left = fromIntegral $ getGuide (leftOf bounds) ly
          right = fromIntegral $ getGuide (rightOf bounds) ly
          bottom = fromIntegral $ getGuide (bottomOf bounds) ly
          w = right - left
          h = bottom - top
          -- FULE uses top-left origin, Brillo uses center origin
          (winW, winH) = stateWindowSize state
          x = left + w / 2 - fromIntegral winW / 2
          y = fromIntegral winH / 2 - top - h / 2
      in  (x, y, w, h)

    translateToCenter :: Float -> Float -> Float -> Float -> Picture -> Picture
    translateToCenter x y _ _ = Translate x y

    brighten :: Color -> Color
    brighten c =
      let (r, g, b, a) = rgbaOfColor c
      in  makeColor (min 1 (r + 0.15)) (min 1 (g + 0.15)) (min 1 (b + 0.15)) a


-- | Handle events
handleEvent :: Event -> AppState -> AppState
handleEvent event state =
  case event of
    -- Feature: Dynamic window resizing
    -- FULE layouts respond to window size changes automatically
    EventResize (newW, newH) ->
      let (ly, comps) = buildLayout newW newH
      in  state
            { stateLayout = ly
            , stateComponents = comps
            , stateWindowSize = (newW, newH)
            }
    -- Handle mouse clicks on panels
    EventKey (MouseButton LeftButton) Down _ pos ->
      state{stateSelectedPanel = findClickedPanel pos state}
    -- Handle mouse movement for hover effects
    EventMotion pos ->
      state{stateHoveredPanel = findHoveredPanel pos state}
    _ -> state


-- | Find which panel was clicked
findClickedPanel :: Point -> AppState -> Maybe T.Text
findClickedPanel (mx, my) state =
  case find isPanelClicked (stateComponents state) of
    Just compInfo ->
      case componentOf compInfo of
        Panel label _ -> Just label
        _ -> Nothing
    Nothing -> Nothing
  where
    isPanelClicked compInfo =
      case componentOf compInfo of
        Panel{} -> pointInBounds (mx, my) (boundsOf compInfo) state
        _ -> False


-- | Find which panel the mouse is hovering over
findHoveredPanel :: Point -> AppState -> Maybe T.Text
findHoveredPanel (mx, my) state =
  case find isPanelHovered (stateComponents state) of
    Just compInfo ->
      case componentOf compInfo of
        Panel label _ -> Just label
        _ -> Nothing
    Nothing -> Nothing
  where
    isPanelHovered compInfo =
      case componentOf compInfo of
        Panel{} -> pointInBounds (mx, my) (boundsOf compInfo) state
        _ -> False


-- | Check if a point is within bounds
pointInBounds :: Point -> Bounds -> AppState -> Bool
pointInBounds (mx, my) bounds state =
  let ly = stateLayout state
      top = fromIntegral $ getGuide (topOf bounds) ly
      left = fromIntegral $ getGuide (leftOf bounds) ly
      right = fromIntegral $ getGuide (rightOf bounds) ly
      bottom = fromIntegral $ getGuide (bottomOf bounds) ly
      (winW, winH) = stateWindowSize state
      -- Convert Brillo coords (center origin) to FULE coords (top-left origin)
      fulex = mx + fromIntegral winW / 2
      fuley = fromIntegral winH / 2 - my
  in  fulex >= left && fulex <= right && fuley >= top && fuley <= bottom


-- | Step function (no animation needed)
stepWorld :: Float -> AppState -> AppState
stepWorld _ = id


-- | Main entry point
main :: IO ()
main =
  play
    (InWindow "FULE Layout Demo" (initialWidth, initialHeight) (100, 100))
    (greyN 0.1) -- Dark background
    60 -- FPS
    initialState
    renderState
    handleEvent
    stepWorld
