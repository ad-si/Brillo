module World (
  Segment,
  World (..),
  initialWorld,
  normaliseWorld,
)
where

import Brillo (Point)
import Data.Vector.Unboxed qualified as V
import Geometry.Randomish (randomishPoints)
import Geometry.Segment (Segment, splitSegmentsOnY, translateSegment)


-- We keep this unpacked so we can use unboxed vector.
-- index, x1, y1, x2, y2
newtype World = World {worldSegments :: V.Vector Segment}


-- | Generate the initial world.
initialWorld :: IO World
initialWorld = do
  let
    n = 100
    minZ = -300
    maxZ = 300

    minDelta = -100
    maxDelta = 100

    centers = randomishPoints 1234 n minZ maxZ
    deltas = randomishPoints 4321 n minDelta maxDelta

    makePoint n' (cX, cY) (dX, dY) =
      (n', (cX, cY), (cX + dX, cY + dY))

    segs = V.zipWith3 makePoint (V.enumFromTo 0 (n - 1)) centers deltas

  return $ World segs


{-| Normalise the world so that the given point is at the origin,
  and split segements that cross the y=0 line.
-}
normaliseWorld :: Point -> World -> World
normaliseWorld (px, py) world = do
  let
    segments_trans =
      V.map (translateSegment (-px) (-py)) $
        worldSegments world
    segments_split =
      splitSegmentsOnY 0 segments_trans

  world{worldSegments = segments_split}
