module Brillo.Data.Picture (
  Picture (..),
  Point,
  Vector,
  Path,

  -- * Shaders
  UniformValue (..),
  ShaderData (..),
  defaultShaderVertex,
  shader,

  -- * Compound shapes
  lineLoop,
  lineLoopAliased,
  circleSolid,
  circleSolidAliased,
  arcSolid,
  arcSolidAliased,
  sectorWire,
  sectorWireAliased,
  rectanglePath,
  rectangleWire,
  rectangleWireAliased,
  rectangleSolid,
  rectangleSolidAliased,
  rectangleUpperPath,
  rectangleUpperWire,
  rectangleUpperWireAliased,
  rectangleUpperSolid,
  rectangleUpperSolidAliased,
)
where

import Brillo.Geometry.Angle
import Brillo.Rendering


-- Shapes ---------------------------------------------------------------------

-- | A closed loop along a path, drawn with anti-aliasing.
lineLoop :: Path -> Picture
lineLoop [] = Line []
lineLoop (x : xs) = Line ((x : xs) ++ [x])


-- | A closed loop along a path, drawn without anti-aliasing.
lineLoopAliased :: Path -> Picture
lineLoopAliased [] = LineAliased []
lineLoopAliased (x : xs) = LineAliased ((x : xs) ++ [x])


-- Circles and Arcs -----------------------------------------------------------

-- | A solid circle with the given radius, drawn with anti-aliasing.
circleSolid :: Float -> Picture
circleSolid r =
  ThickCircle (r / 2) r


-- | A solid circle with the given radius, drawn without anti-aliasing.
circleSolidAliased :: Float -> Picture
circleSolidAliased r =
  ThickCircleAliased (r / 2) r


-- | A solid arc, drawn counter-clockwise between two angles (in degrees) at the given radius, with anti-aliasing.
arcSolid :: Float -> Float -> Float -> Picture
arcSolid a1 a2 r =
  ThickArc a1 a2 (r / 2) r


-- | A solid arc, drawn counter-clockwise between two angles (in degrees) at the given radius, without anti-aliasing.
arcSolidAliased :: Float -> Float -> Float -> Picture
arcSolidAliased a1 a2 r =
  ThickArcAliased a1 a2 (r / 2) r


{-| A wireframe sector of a circle, drawn with anti-aliasing.
  An arc is draw counter-clockwise from the first to the second angle (in degrees) at
  the given radius. Lines are drawn from the origin to the ends of the arc.
-}

---
--   NOTE: We take the absolute value of the radius incase it's negative.
--   It would also make sense to draw the sector flipped around the
--   origin, but I think taking the absolute value will be less surprising
--   for the user.
--
sectorWire :: Float -> Float -> Float -> Picture
sectorWire a1 a2 r_ =
  let r = abs r_
  in  Pictures
        [ Arc a1 a2 r
        , Line [(0, 0), (r * cos (degToRad a1), r * sin (degToRad a1))]
        , Line [(0, 0), (r * cos (degToRad a2), r * sin (degToRad a2))]
        ]


{-| A wireframe sector of a circle, drawn without anti-aliasing.
  An arc is draw counter-clockwise from the first to the second angle (in degrees) at
  the given radius. Lines are drawn from the origin to the ends of the arc.
-}
sectorWireAliased :: Float -> Float -> Float -> Picture
sectorWireAliased a1 a2 r_ =
  let r = abs r_
  in  Pictures
        [ ArcAliased a1 a2 r
        , LineAliased [(0, 0), (r * cos (degToRad a1), r * sin (degToRad a1))]
        , LineAliased [(0, 0), (r * cos (degToRad a2), r * sin (degToRad a2))]
        ]


-- Rectangles -----------------------------------------------------------------
-- NOTE: Only the first of these rectangle functions has haddocks on the
--       arguments to reduce the amount of noise in the extracted docs.

-- | A path representing a rectangle centered about the origin
rectanglePath ::
  -- | width of rectangle
  Float ->
  -- | height of rectangle
  Float ->
  Path
rectanglePath sizeX sizeY =
  let sx = sizeX / 2
      sy = sizeY / 2
  in  [(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]


-- | A wireframe rectangle centered about the origin, drawn with anti-aliasing.
rectangleWire :: Float -> Float -> Picture
rectangleWire sizeX sizeY =
  lineLoop $ rectanglePath sizeX sizeY


-- | A wireframe rectangle centered about the origin, drawn without anti-aliasing.
rectangleWireAliased :: Float -> Float -> Picture
rectangleWireAliased sizeX sizeY =
  lineLoopAliased $ rectanglePath sizeX sizeY


-- | A wireframe rectangle in the y > 0 half of the x-y plane, drawn with anti-aliasing.
rectangleUpperWire :: Float -> Float -> Picture
rectangleUpperWire sizeX sizeY =
  lineLoop $ rectangleUpperPath sizeX sizeY


-- | A wireframe rectangle in the y > 0 half of the x-y plane, drawn without anti-aliasing.
rectangleUpperWireAliased :: Float -> Float -> Picture
rectangleUpperWireAliased sizeX sizeY =
  lineLoopAliased $ rectangleUpperPath sizeX sizeY


-- | A path representing a rectangle in the y > 0 half of the x-y plane.
rectangleUpperPath :: Float -> Float -> Path
rectangleUpperPath sizeX sy =
  let sx = sizeX / 2
  in  [(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]


-- | A solid rectangle centered about the origin, drawn with anti-aliased edges.
rectangleSolid :: Float -> Float -> Picture
rectangleSolid sizeX sizeY =
  Polygon $ rectanglePath sizeX sizeY


-- | A solid rectangle centered about the origin, drawn without anti-aliasing.
rectangleSolidAliased :: Float -> Float -> Picture
rectangleSolidAliased sizeX sizeY =
  PolygonAliased $ rectanglePath sizeX sizeY


-- | A solid rectangle in the y > 0 half of the x-y plane, drawn with anti-aliased edges.
rectangleUpperSolid :: Float -> Float -> Picture
rectangleUpperSolid sizeX sizeY =
  Polygon $ rectangleUpperPath sizeX sizeY


-- | A solid rectangle in the y > 0 half of the x-y plane, drawn without anti-aliasing.
rectangleUpperSolidAliased :: Float -> Float -> Picture
rectangleUpperSolidAliased sizeX sizeY =
  PolygonAliased $ rectangleUpperPath sizeX sizeY
