module Brillo.Data.Picture (
  Picture (..),
  Point,
  Vector,
  Path,

  -- * Aliases for Picture constructors
  blank,
  polygon,
  polygonAliased,
  line,
  lineAliased,
  thickLine,
  thickLineAliased,
  circle,
  circleAliased,
  thickCircle,
  thickCircleAliased,
  arc,
  arcAliased,
  thickArc,
  thickArcAliased,
  text,
  textAliased,
  thickText,
  thickTextAliased,
  truetypeText,
  bitmap,
  bitmapSection,
  -- , bitmap
  color,
  translate,
  rotate,
  scale,
  pictures,

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
import Data.Text (Text)


-- Constructors ----------------------------------------------------------------
-- NOTE: The docs here should be identical to the ones on the constructors.

-- | A blank picture, with nothing in it.
blank :: Picture
blank = Blank


-- | A polygon filled with a solid color, drawn with anti-aliased edges.
polygon :: Path -> Picture
polygon = Polygon


-- | A polygon filled with a solid color, drawn without anti-aliasing.
polygonAliased :: Path -> Picture
polygonAliased = PolygonAliased


-- | A line along an arbitrary path, drawn with anti-aliasing.
line :: Path -> Picture
line = Line


-- | A line along an arbitrary path, drawn without anti-aliasing.
lineAliased :: Path -> Picture
lineAliased = LineAliased


-- | A line along an arbitrary path with a given thickness, drawn with anti-aliasing.
thickLine :: Path -> Float -> Picture
thickLine = ThickLine


-- | A line along an arbitrary path with a given thickness, drawn without anti-aliasing.
thickLineAliased :: Path -> Float -> Picture
thickLineAliased = ThickLineAliased


-- | A circle with the given radius, drawn with anti-aliasing.
circle :: Float -> Picture
circle = Circle


-- | A circle with the given radius, drawn without anti-aliasing.
circleAliased :: Float -> Picture
circleAliased = CircleAliased


{-| A circle with the given thickness and radius, drawn with anti-aliasing.
  If the thickness is 0 then this is equivalent to `Circle`.
-}
thickCircle :: Float -> Float -> Picture
thickCircle = ThickCircle


{-| A circle with the given thickness and radius, drawn without anti-aliasing.
  If the thickness is 0 then this is equivalent to `CircleAliased`.
-}
thickCircleAliased :: Float -> Float -> Picture
thickCircleAliased = ThickCircleAliased


{-| A circular arc drawn counter-clockwise between two angles (in degrees)
  at the given radius, drawn with anti-aliasing.
-}
arc :: Float -> Float -> Float -> Picture
arc = Arc


{-| A circular arc drawn counter-clockwise between two angles (in degrees)
  at the given radius, drawn without anti-aliasing.
-}
arcAliased :: Float -> Float -> Float -> Picture
arcAliased = ArcAliased


{-| A circular arc drawn counter-clockwise between two angles (in degrees),
  with the given radius and thickness, drawn with anti-aliasing.
  If the thickness is 0 then this is equivalent to `Arc`.
-}
thickArc :: Float -> Float -> Float -> Float -> Picture
thickArc = ThickArc


{-| A circular arc drawn counter-clockwise between two angles (in degrees),
  with the given radius and thickness, drawn without anti-aliasing.
  If the thickness is 0 then this is equivalent to `ArcAliased`.
-}
thickArcAliased :: Float -> Float -> Float -> Float -> Picture
thickArcAliased = ThickArcAliased


-- | Some text to draw with a vector font, drawn with anti-aliasing.
text :: Text -> Picture
text = Text


-- | Some text to draw with a vector font, drawn without anti-aliasing.
textAliased :: Text -> Picture
textAliased = TextAliased


-- | Some text to draw with a vector font and a given thickness, drawn with anti-aliasing.
thickText :: Text -> Float -> Picture
thickText = ThickText


-- | Some text to draw with a vector font and a given thickness, drawn without anti-aliasing.
thickTextAliased :: Text -> Float -> Picture
thickTextAliased = ThickTextAliased


-- | Some text to draw with a TrueType font, using the given pixel height.
truetypeText :: FilePath -> Int -> Text -> Picture
truetypeText = TrueTypeText


-- | A bitmap image
bitmap :: BitmapData -> Picture
bitmap bitmapData = Bitmap bitmapData


{-| a subsection of a bitmap image
  first argument selects a sub section in the bitmap
  second argument determines the bitmap data
-}
bitmapSection :: Rectangle -> BitmapData -> Picture
bitmapSection = BitmapSection


-- | A picture drawn with this color.
color :: Color -> Picture -> Picture
color = Color


-- | A picture translated by the given x and y coordinates.
translate :: Float -> Float -> Picture -> Picture
translate = Translate


-- | A picture rotated clockwise by the given angle (in degrees).
rotate :: Float -> Picture -> Picture
rotate = Rotate


-- | A picture scaled by the given x and y factors.
scale :: Float -> Float -> Picture -> Picture
scale = Scale


-- | A picture consisting of several others.
pictures :: [Picture] -> Picture
pictures = Pictures


-- Other Shapes ---------------------------------------------------------------

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
  thickCircle (r / 2) r


-- | A solid circle with the given radius, drawn without anti-aliasing.
circleSolidAliased :: Float -> Picture
circleSolidAliased r =
  thickCircleAliased (r / 2) r


-- | A solid arc, drawn counter-clockwise between two angles (in degrees) at the given radius, with anti-aliasing.
arcSolid :: Float -> Float -> Float -> Picture
arcSolid a1 a2 r =
  thickArc a1 a2 (r / 2) r


-- | A solid arc, drawn counter-clockwise between two angles (in degrees) at the given radius, without anti-aliasing.
arcSolidAliased :: Float -> Float -> Float -> Picture
arcSolidAliased a1 a2 r =
  thickArcAliased a1 a2 (r / 2) r


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
