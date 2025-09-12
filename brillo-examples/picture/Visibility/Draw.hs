{-# LANGUAGE PatternGuards #-}

module Draw (
  drawState,
  drawWorld,
)
where

import Brillo (
  Picture (Color, Line, Pictures, ThickCircle, Translate),
  Point,
  blank,
  dim,
  green,
  greyN,
  rectangleSolid,
  red,
  white,
 )
import Brillo.Geometry.Line (intersectSegSeg)
import Data.Maybe (isJust)
import Data.Vector.Unboxed qualified as V
import State (
  ModeDisplay (ModeDisplayNormalised, ModeDisplayWorld),
  ModeOverlay (ModeOverlayVisApprox),
  State (
    stateModeDisplay,
    stateModeOverlay,
    stateTargetPos,
    stateViewPos,
    stateWorld
  ),
 )
import World (Segment, World (worldSegments), normaliseWorld)


drawState :: State -> Picture
drawState state
  | ModeDisplayWorld <- stateModeDisplay state =
      drawWorldWithViewPos
        (stateModeOverlay state)
        (stateViewPos state)
        (stateTargetPos state)
        (stateWorld state)
  | ModeDisplayNormalised <- stateModeDisplay state =
      drawWorldWithViewPos
        (stateModeOverlay state)
        (0, 0)
        Nothing
        $ normaliseWorld (stateViewPos state)
        $ stateWorld state


drawWorldWithViewPos :: ModeOverlay -> Point -> Maybe Point -> World -> Picture
drawWorldWithViewPos
  modeOverlay
  pView@(vx, vy)
  mTarget
  world =
    let
      -- the world
      picWorld =
        Color white $
          drawWorld world

      -- view position indicator
      picView =
        Color red $
          Translate vx vy $
            ThickCircle 2 4

      -- target position indicator
      picTargets
        | Just pTarget@(px, py) <- mTarget =
            let picTarget = Translate px py $ ThickCircle 2 4

                -- line between view and target pos
                picLine = Line [pView, pTarget]

                picSegsHit =
                  Pictures $
                    [ Line [p1, p2]
                    | (_, p1, p2) <- V.toList $ worldSegments world
                    , isJust $ intersectSegSeg p1 p2 pView pTarget
                    ]
            in  Color red $ Pictures [picTarget, picLine, picSegsHit]
        | otherwise =
            blank

      -- overlay
      picOverlay
        | ModeOverlayVisApprox <- modeOverlay =
            drawVisGrid 10 pView world
        | otherwise =
            blank
    in
      Pictures [picOverlay, picWorld, picView, picTargets]


-- | Draw a grid of points showing what is visible from a view position
drawVisGrid :: Float -> Point -> World -> Picture
drawVisGrid cellSize pView world = do
  let
    visible pTarget =
      not $
        any
          (isJust . (\(_, p1, p2) -> intersectSegSeg pView pTarget p1 p2))
          (V.toList $ worldSegments world)

  Pictures $
    [ if visible (x, y)
      then Color (dim green) $ Translate x y $ rectangleSolid cellSize cellSize
      else Color (greyN 0.2) $ Translate x y $ rectangleSolid cellSize cellSize
    | x <- [-400, -400 + cellSize .. 400]
    , y <- [-400, -400 + cellSize .. 400]
    ]


-- | Draw the segments in the world.
drawWorld :: World -> Picture
drawWorld world =
  drawSegments $
    worldSegments world


-- | Draw an array of segments.
drawSegments :: V.Vector Segment -> Picture
drawSegments segments =
  Pictures $
    map drawSegment $
      V.toList segments


-- | Draw a single segment.
drawSegment :: Segment -> Picture
drawSegment (_, (x1, y1), (x2, y2)) = do
  let f = fromRational . toRational
  Line [(f x1, f y1), (f x2, f y2)]
