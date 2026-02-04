module Brillo.Data.Picture (
  Picture (..),
  Point,
  Vector,
  Path,

  -- * Aliases for Picture constructors
  blank,
  polygon,
  polygonSmooth,
  line,
  lineSmooth,
  thickLine,
  thickLineSmooth,
  circle,
  circleSmooth,
  thickCircle,
  thickCircleSmooth,
  arc,
  arcSmooth,
  thickArc,
  thickArcSmooth,
  text,
  textSmooth,
  thickText,
  thickTextSmooth,
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
  lineLoopSmooth,
  circleSolid,
  circleSolidSmooth,
  arcSolid,
  arcSolidSmooth,
  sectorWire,
  sectorWireSmooth,
  rectanglePath,
  rectangleWire,
  rectangleWireSmooth,
  rectangleSolid,
  rectangleSolidSmooth,
  rectangleUpperPath,
  rectangleUpperWire,
  rectangleUpperWireSmooth,
  rectangleUpperSolid,
  rectangleUpperSolidSmooth,
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


-- | A polygon filled with a solid color.
polygon :: Path -> Picture
polygon = Polygon


-- | A polygon filled with a solid color, drawn with anti-aliased edges.
polygonSmooth :: Path -> Picture
polygonSmooth = PolygonSmooth


-- | A line along an arbitrary path.
line :: Path -> Picture
line = Line


-- | A line along an arbitrary path, drawn with anti-aliasing.
lineSmooth :: Path -> Picture
lineSmooth = LineSmooth


-- | A line along an arbitrary path with a given thickness.
thickLine :: Path -> Float -> Picture
thickLine = ThickLine


-- | A line along an arbitrary path with a given thickness, drawn with anti-aliasing.
thickLineSmooth :: Path -> Float -> Picture
thickLineSmooth = ThickLineSmooth


-- | A circle with the given radius.
circle :: Float -> Picture
circle = Circle


-- | A circle with the given radius, drawn with anti-aliasing.
circleSmooth :: Float -> Picture
circleSmooth = CircleSmooth


{-| A circle with the given thickness and radius.
  If the thickness is 0 then this is equivalent to `Circle`.
-}
thickCircle :: Float -> Float -> Picture
thickCircle = ThickCircle


{-| A circle with the given thickness and radius, drawn with anti-aliasing.
  If the thickness is 0 then this is equivalent to `CircleSmooth`.
-}
thickCircleSmooth :: Float -> Float -> Picture
thickCircleSmooth = ThickCircleSmooth


{-| A circular arc drawn counter-clockwise between two angles (in degrees)
  at the given radius.
-}
arc :: Float -> Float -> Float -> Picture
arc = Arc


{-| A circular arc drawn counter-clockwise between two angles (in degrees)
  at the given radius, drawn with anti-aliasing.
-}
arcSmooth :: Float -> Float -> Float -> Picture
arcSmooth = ArcSmooth


{-| A circular arc drawn counter-clockwise between two angles (in degrees),
  with the given radius  and thickness.
  If the thickness is 0 then this is equivalent to `Arc`.
-}
thickArc :: Float -> Float -> Float -> Float -> Picture
thickArc = ThickArc


{-| A circular arc drawn counter-clockwise between two angles (in degrees),
  with the given radius and thickness, drawn with anti-aliasing.
  If the thickness is 0 then this is equivalent to `ArcSmooth`.
-}
thickArcSmooth :: Float -> Float -> Float -> Float -> Picture
thickArcSmooth = ThickArcSmooth


-- | Some text to draw with a vector font.
text :: Text -> Picture
text = Text


-- | Some text to draw with a vector font, drawn with anti-aliasing.
textSmooth :: Text -> Picture
textSmooth = TextSmooth


-- | Some text to draw with a vector font and a given thickness.
thickText :: Text -> Float -> Picture
thickText = ThickText


-- | Some text to draw with a vector font and a given thickness, drawn with anti-aliasing.
thickTextSmooth :: Text -> Float -> Picture
thickTextSmooth = ThickTextSmooth


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

-- | A closed loop along a path.
lineLoop :: Path -> Picture
lineLoop [] = Line []
lineLoop (x : xs) = Line ((x : xs) ++ [x])


-- | A closed loop along a path, drawn with anti-aliasing.
lineLoopSmooth :: Path -> Picture
lineLoopSmooth [] = LineSmooth []
lineLoopSmooth (x : xs) = LineSmooth ((x : xs) ++ [x])


-- Circles and Arcs -----------------------------------------------------------

-- | A solid circle with the given radius.
circleSolid :: Float -> Picture
circleSolid r =
  thickCircle (r / 2) r


-- | A solid circle with the given radius, drawn with anti-aliasing.
circleSolidSmooth :: Float -> Picture
circleSolidSmooth r =
  thickCircleSmooth (r / 2) r


-- | A solid arc, drawn counter-clockwise between two angles (in degrees) at the given radius.
arcSolid :: Float -> Float -> Float -> Picture
arcSolid a1 a2 r =
  thickArc a1 a2 (r / 2) r


-- | A solid arc, drawn counter-clockwise between two angles (in degrees) at the given radius, with anti-aliasing.
arcSolidSmooth :: Float -> Float -> Float -> Picture
arcSolidSmooth a1 a2 r =
  thickArcSmooth a1 a2 (r / 2) r


{-| A wireframe sector of a circle.
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


{-| A wireframe sector of a circle, drawn with anti-aliasing.
  An arc is draw counter-clockwise from the first to the second angle (in degrees) at
  the given radius. Lines are drawn from the origin to the ends of the arc.
-}
sectorWireSmooth :: Float -> Float -> Float -> Picture
sectorWireSmooth a1 a2 r_ =
  let r = abs r_
  in  Pictures
        [ ArcSmooth a1 a2 r
        , LineSmooth [(0, 0), (r * cos (degToRad a1), r * sin (degToRad a1))]
        , LineSmooth [(0, 0), (r * cos (degToRad a2), r * sin (degToRad a2))]
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


-- | A wireframe rectangle centered about the origin.
rectangleWire :: Float -> Float -> Picture
rectangleWire sizeX sizeY =
  lineLoop $ rectanglePath sizeX sizeY


-- | A wireframe rectangle centered about the origin, drawn with anti-aliasing.
rectangleWireSmooth :: Float -> Float -> Picture
rectangleWireSmooth sizeX sizeY =
  lineLoopSmooth $ rectanglePath sizeX sizeY


-- | A wireframe rectangle in the y > 0 half of the x-y plane.
rectangleUpperWire :: Float -> Float -> Picture
rectangleUpperWire sizeX sizeY =
  lineLoop $ rectangleUpperPath sizeX sizeY


-- | A wireframe rectangle in the y > 0 half of the x-y plane, drawn with anti-aliasing.
rectangleUpperWireSmooth :: Float -> Float -> Picture
rectangleUpperWireSmooth sizeX sizeY =
  lineLoopSmooth $ rectangleUpperPath sizeX sizeY


-- | A path representing a rectangle in the y > 0 half of the x-y plane.
rectangleUpperPath :: Float -> Float -> Path
rectangleUpperPath sizeX sy =
  let sx = sizeX / 2
  in  [(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]


-- | A solid rectangle centered about the origin.
rectangleSolid :: Float -> Float -> Picture
rectangleSolid sizeX sizeY =
  Polygon $ rectanglePath sizeX sizeY


-- | A solid rectangle centered about the origin, drawn with anti-aliased edges.
rectangleSolidSmooth :: Float -> Float -> Picture
rectangleSolidSmooth sizeX sizeY =
  PolygonSmooth $ rectanglePath sizeX sizeY


-- | A solid rectangle in the y > 0 half of the x-y plane.
rectangleUpperSolid :: Float -> Float -> Picture
rectangleUpperSolid sizeX sizeY =
  Polygon $ rectangleUpperPath sizeX sizeY


-- | A solid rectangle in the y > 0 half of the x-y plane, drawn with anti-aliased edges.
rectangleUpperSolidSmooth :: Float -> Float -> Picture
rectangleUpperSolidSmooth sizeX sizeY =
  PolygonSmooth $ rectangleUpperPath sizeX sizeY
