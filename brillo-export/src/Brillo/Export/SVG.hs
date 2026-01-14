{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Export Brillo pictures to SVG format.

This module provides functions to convert Brillo 'Picture' values
to SVG (Scalable Vector Graphics) format. SVG is a vector graphics
format that can be scaled without loss of quality.
-}
module Brillo.Export.SVG (
  -- * Export functions
  exportPictureToSVG,
  exportPicturesToSVG,

  -- * Direct rendering
  renderPictureToSVG,
  pictureToSVGDoc,
) where

import Brillo.Rendering (
  BitmapData (..),
  Color,
  Picture (..),
  Point,
  Rectangle (..),
  makeColor,
  rgbaOfColor,
 )
import Codec.Picture qualified as JP
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector.Storable qualified as V
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, peekElemOff)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)


-- | Size of the output image (width, height) in pixels
type Size = (Int, Int)


-- | An animation function that takes time and returns a Picture
type Animation = Float -> Picture


-- | Export a Brillo Picture to an SVG file.
exportPictureToSVG ::
  -- | (width, height) in pixels
  Size ->
  -- | Background color
  Color ->
  -- | Output file path
  FilePath ->
  -- | Picture to export
  Picture ->
  IO ()
exportPictureToSVG size bgColor filePath picture = do
  let svg = pictureToSVGDoc size bgColor picture
  TIO.writeFile filePath svg


-- | Export a series of Brillo Pictures to SVG files.
-- The file path pattern should contain "%d" which will be replaced by the frame number.
exportPicturesToSVG ::
  -- | (width, height) in pixels
  Size ->
  -- | Background color
  Color ->
  -- | File path pattern (must contain "%d")
  FilePath ->
  -- | Animation function
  Animation ->
  -- | List of time points
  [Float] ->
  IO ()
exportPicturesToSVG size bgColor filePathPattern animation times = do
  mapM_ exportFrame (zip [1 ..] times)
 where
  exportFrame (n :: Int, t) = do
    let picture = animation t
    let filePath = printf filePathPattern n
    exportPictureToSVG size bgColor filePath picture


-- | Render a Picture to SVG text.
-- Returns just the SVG content without the XML declaration and doctype.
renderPictureToSVG ::
  -- | (width, height) in pixels
  Size ->
  -- | Background color
  Color ->
  -- | Picture to render
  Picture ->
  Text
renderPictureToSVG (width, height) bgColor picture =
  let
    -- SVG coordinate system has origin at top-left, y increases downward
    -- Brillo has origin at center, y increases upward
    -- We need to transform accordingly
    halfW = fromIntegral width / 2 :: Float
    halfH = fromIntegral height / 2 :: Float

    -- Create the transformed picture (flip y-axis and translate origin)
    transformedPicture = Translate halfW halfH (Scale 1 (-1) picture)

    -- Render background
    bgRect = renderBackground width height bgColor

    -- Render the picture content
    content = renderPicture defaultRenderState transformedPicture
   in
    T.concat [bgRect, content]


-- | Convert a Picture to a complete SVG document.
pictureToSVGDoc ::
  -- | (width, height) in pixels
  Size ->
  -- | Background color
  Color ->
  -- | Picture to convert
  Picture ->
  Text
pictureToSVGDoc size@(width, height) bgColor picture =
  T.concat
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<svg xmlns=\"http://www.w3.org/2000/svg\" "
    , "xmlns:xlink=\"http://www.w3.org/1999/xlink\" "
    , "width=\""
    , T.pack (show width)
    , "\" height=\""
    , T.pack (show height)
    , "\" viewBox=\"0 0 "
    , T.pack (show width)
    , " "
    , T.pack (show height)
    , "\">\n"
    , renderPictureToSVG size bgColor picture
    , "</svg>\n"
    ]


-- | State for rendering, tracks current color
data RenderState = RenderState
  { rsColor :: Color
  -- ^ Current fill/stroke color
  }


-- | Default render state with black color
defaultRenderState :: RenderState
defaultRenderState =
  RenderState
    { rsColor = makeColor 0 0 0 1
    }


-- | Render the background rectangle
renderBackground :: Int -> Int -> Color -> Text
renderBackground width height bgColor =
  T.concat
    [ "  <rect x=\"0\" y=\"0\" width=\""
    , T.pack (show width)
    , "\" height=\""
    , T.pack (show height)
    , "\" fill=\""
    , colorToSVG bgColor
    , "\"/>\n"
    ]


-- | Convert a Brillo Color to SVG color string
colorToSVG :: Color -> Text
colorToSVG color =
  let (r, g, b, a) = rgbaOfColor color
      ri = round (r * 255) :: Int
      gi = round (g * 255) :: Int
      bi = round (b * 255) :: Int
   in if a >= 1.0
        then T.pack $ printf "rgb(%d,%d,%d)" ri gi bi
        else T.pack $ printf "rgba(%d,%d,%d,%.3f)" ri gi bi a


-- | Get opacity from color
colorOpacity :: Color -> Float
colorOpacity color =
  let (_, _, _, a) = rgbaOfColor color
   in a


-- | Main picture rendering function
renderPicture :: RenderState -> Picture -> Text
renderPicture _ Blank = ""
renderPicture rs (Polygon path) = renderPolygon rs path
renderPicture rs (Line path) = renderLine rs path 1.0
renderPicture rs (LineSmooth path) = renderSmoothLine rs path 1.0
renderPicture rs (ThickLine path thickness) = renderLine rs path thickness
renderPicture rs (ThickLineSmooth path thickness) = renderSmoothLine rs path thickness
renderPicture rs (Circle radius) = renderCircle rs radius 1.0
renderPicture rs (ThickCircle thickness radius) = renderThickCircle rs thickness radius
renderPicture rs (Arc startAngle endAngle radius) = renderArc rs startAngle endAngle radius 1.0
renderPicture rs (ThickArc startAngle endAngle thickness radius) = renderArc rs startAngle endAngle radius thickness
renderPicture rs (Text txt) = renderText rs txt 1.0
renderPicture rs (ThickText txt thickness) = renderText rs txt thickness
renderPicture rs (TrueTypeText _fontPath _size txt) = renderText rs txt 1.0
renderPicture rs (Bitmap bitmapData) = renderBitmap rs bitmapData Nothing
renderPicture rs (BitmapSection rect bitmapData) = renderBitmap rs bitmapData (Just rect)
renderPicture rs (Color color picture) =
  renderPicture (rs{rsColor = color}) picture
renderPicture rs (Translate dx dy picture) =
  T.concat
    [ "  <g transform=\"translate("
    , T.pack (showFloat dx)
    , ","
    , T.pack (showFloat dy)
    , ")\">\n"
    , renderPicture rs picture
    , "  </g>\n"
    ]
renderPicture rs (Rotate angle picture) =
  -- Brillo uses clockwise rotation in degrees
  -- SVG uses clockwise rotation in degrees too
  T.concat
    [ "  <g transform=\"rotate("
    , T.pack (showFloat (-angle))
    , ")\">\n"
    , renderPicture rs picture
    , "  </g>\n"
    ]
renderPicture rs (Scale sx sy picture) =
  T.concat
    [ "  <g transform=\"scale("
    , T.pack (showFloat sx)
    , ","
    , T.pack (showFloat sy)
    , ")\">\n"
    , renderPicture rs picture
    , "  </g>\n"
    ]
renderPicture rs (Pictures pictures) =
  T.concat (map (renderPicture rs) pictures)


-- | Render a filled polygon
renderPolygon :: RenderState -> [Point] -> Text
renderPolygon _ [] = ""
renderPolygon rs points =
  T.concat
    [ "  <polygon points=\""
    , T.pack $ unwords [showFloat x ++ "," ++ showFloat y | (x, y) <- points]
    , "\" fill=\""
    , colorToSVG (rsColor rs)
    , "\""
    , opacityAttr (rsColor rs)
    , "/>\n"
    ]


-- | Render a line (polyline)
renderLine :: RenderState -> [Point] -> Float -> Text
renderLine _ [] _ = ""
renderLine _ [_] _ = ""
renderLine rs points thickness =
  T.concat
    [ "  <polyline points=\""
    , T.pack $ unwords [showFloat x ++ "," ++ showFloat y | (x, y) <- points]
    , "\" fill=\"none\" stroke=\""
    , colorToSVG (rsColor rs)
    , "\" stroke-width=\""
    , T.pack (showFloat (max 1.0 thickness))
    , "\""
    , strokeOpacityAttr (rsColor rs)
    , "/>\n"
    ]


-- | Render a smooth line using cubic bezier curves
renderSmoothLine :: RenderState -> [Point] -> Float -> Text
renderSmoothLine _ [] _ = ""
renderSmoothLine _ [_] _ = ""
renderSmoothLine rs points thickness =
  let pathData = smoothPathToSVG points
   in T.concat
        [ "  <path d=\""
        , pathData
        , "\" fill=\"none\" stroke=\""
        , colorToSVG (rsColor rs)
        , "\" stroke-width=\""
        , T.pack (showFloat (max 1.0 thickness))
        , "\" stroke-linecap=\"round\" stroke-linejoin=\"round\""
        , strokeOpacityAttr (rsColor rs)
        , "/>\n"
        ]


-- | Convert a path to a smooth SVG path using cubic bezier curves
smoothPathToSVG :: [Point] -> Text
smoothPathToSVG [] = ""
smoothPathToSVG [p] = T.pack $ "M " ++ showPoint p
smoothPathToSVG [p1, p2] = T.pack $ "M " ++ showPoint p1 ++ " L " ++ showPoint p2
smoothPathToSVG (p1 : p2 : rest) =
  T.pack $
    "M "
      ++ showPoint p1
      ++ " "
      ++ smoothCurves (p1 : p2 : rest)
 where
  smoothCurves :: [Point] -> String
  smoothCurves pts
    | length pts < 3 = ""
    | otherwise = intercalate " " $ zipWith3 makeCurve pts (tail pts) (drop 2 pts)

  makeCurve :: Point -> Point -> Point -> String
  makeCurve (_x0, _y0) (x1, y1) (x2, y2) =
    let cx2 = (x1 + x2) / 2
        cy2 = (y1 + y2) / 2
     in "Q " ++ showFloat x1 ++ " " ++ showFloat y1 ++ " " ++ showFloat cx2 ++ " " ++ showFloat cy2


-- | Render a circle outline
renderCircle :: RenderState -> Float -> Float -> Text
renderCircle rs radius thickness =
  T.concat
    [ "  <circle cx=\"0\" cy=\"0\" r=\""
    , T.pack (showFloat (abs radius))
    , "\" fill=\"none\" stroke=\""
    , colorToSVG (rsColor rs)
    , "\" stroke-width=\""
    , T.pack (showFloat (max 1.0 thickness))
    , "\""
    , strokeOpacityAttr (rsColor rs)
    , "/>\n"
    ]


-- | Render a thick circle (annulus/ring)
renderThickCircle :: RenderState -> Float -> Float -> Text
renderThickCircle rs thickness radius
  | thickness <= 0 = renderCircle rs radius 1.0
  | otherwise =
      -- ThickCircle draws a ring with inner radius (radius - thickness/2)
      -- and outer radius (radius + thickness/2)
      let innerRadius = max 0 (abs radius - thickness / 2)
          outerRadius = abs radius + thickness / 2
       in T.concat
            [ "  <circle cx=\"0\" cy=\"0\" r=\""
            , T.pack (showFloat ((innerRadius + outerRadius) / 2))
            , "\" fill=\"none\" stroke=\""
            , colorToSVG (rsColor rs)
            , "\" stroke-width=\""
            , T.pack (showFloat (outerRadius - innerRadius))
            , "\""
            , strokeOpacityAttr (rsColor rs)
            , "/>\n"
            ]


-- | Render an arc
renderArc :: RenderState -> Float -> Float -> Float -> Float -> Text
renderArc rs startAngle endAngle radius thickness =
  let
    -- Convert angles from degrees to radians
    -- Brillo uses counter-clockwise from the positive x-axis
    -- SVG arcs need special handling
    r = abs radius
    startRad = startAngle * pi / 180
    endRad = endAngle * pi / 180

    -- Calculate start and end points
    x1 = r * cos startRad
    y1 = r * sin startRad
    x2 = r * cos endRad
    y2 = r * sin endRad

    -- Determine arc sweep (large arc flag and sweep direction)
    angleDiff = endAngle - startAngle
    largeArc = if abs angleDiff > 180 then 1 else 0 :: Int
    sweep = if angleDiff > 0 then 1 else 0 :: Int

    pathData =
      "M "
        ++ showFloat x1
        ++ " "
        ++ showFloat y1
        ++ " A "
        ++ showFloat r
        ++ " "
        ++ showFloat r
        ++ " 0 "
        ++ show largeArc
        ++ " "
        ++ show sweep
        ++ " "
        ++ showFloat x2
        ++ " "
        ++ showFloat y2
   in
    T.concat
      [ "  <path d=\""
      , T.pack pathData
      , "\" fill=\"none\" stroke=\""
      , colorToSVG (rsColor rs)
      , "\" stroke-width=\""
      , T.pack (showFloat (max 1.0 thickness))
      , "\" stroke-linecap=\"round\""
      , strokeOpacityAttr (rsColor rs)
      , "/>\n"
      ]


-- | Render text
renderText :: RenderState -> Text -> Float -> Text
renderText rs txt thickness =
  T.concat
    [ "  <text x=\"0\" y=\"0\" fill=\""
    , colorToSVG (rsColor rs)
    , "\" font-family=\"monospace\" font-size=\"100\""
    , if thickness > 1.0
        then
          T.concat
            [ " stroke=\""
            , colorToSVG (rsColor rs)
            , "\" stroke-width=\""
            , T.pack (showFloat (thickness - 1.0))
            , "\""
            ]
        else ""
    , opacityAttr (rsColor rs)
    , ">"
    , escapeXML txt
    , "</text>\n"
    ]


-- | Render a bitmap image as a data URI embedded image
renderBitmap :: RenderState -> BitmapData -> Maybe Rectangle -> Text
renderBitmap _ bitmapData maybeRect =
  let
    BitmapData _len _fmt (bmpWidth, bmpHeight) _cache fptr = bitmapData

    -- Get the actual section to render
    (srcX, srcY, width, height) = case maybeRect of
      Nothing -> (0, 0, bmpWidth, bmpHeight)
      Just (Rectangle (rx, ry) (rw, rh)) -> (rx, ry, rw, rh)

    -- Create a base64 PNG data URI
    dataUri = unsafePerformIO $ do
      withForeignPtr fptr $ \ptr -> do
        -- Read pixel data
        let pixelCount = bmpWidth * bmpHeight * 4
        pixelData <- V.generateM pixelCount $ \i -> do
          let bytePtr = castPtr ptr :: Ptr Word8
          peekElemOff bytePtr i

        -- Create JuicyPixels image
        let img = JP.Image bmpWidth bmpHeight pixelData :: JP.Image JP.PixelRGBA8

        -- Encode to PNG and then base64
        let pngBytes = JP.encodePng img
        pure $ "data:image/png;base64," ++ encodeBase64 (BL.unpack pngBytes)

    -- Center the image at origin (Brillo convention)
    halfW = fromIntegral width / 2 :: Float
    halfH = fromIntegral height / 2 :: Float
   in
    T.concat
      [ "  <image x=\""
      , T.pack (showFloat (-halfW))
      , "\" y=\""
      , T.pack (showFloat (-halfH))
      , "\" width=\""
      , T.pack (show width)
      , "\" height=\""
      , T.pack (show height)
      , "\" xlink:href=\""
      , T.pack dataUri
      , "\""
      , if srcX /= 0 || srcY /= 0
          then
            T.concat
              [ " preserveAspectRatio=\"xMinYMin slice\" viewBox=\""
              , T.pack (show srcX)
              , " "
              , T.pack (show srcY)
              , " "
              , T.pack (show width)
              , " "
              , T.pack (show height)
              , "\""
              ]
          else ""
      , "/>\n"
      ]


-- | Simple base64 encoding
encodeBase64 :: [Word8] -> String
encodeBase64 = go
 where
  go [] = ""
  go [a] =
    let (i1, i2) = (fromIntegral a `div` 4, (fromIntegral a `mod` 4) * 16)
     in [base64Char i1, base64Char i2, '=', '=']
  go [a, b] =
    let i1 = fromIntegral a `div` 4
        i2 = ((fromIntegral a `mod` 4) * 16) + (fromIntegral b `div` 16)
        i3 = (fromIntegral b `mod` 16) * 4
     in [base64Char i1, base64Char i2, base64Char i3, '=']
  go (a : b : c : rest) =
    let i1 = fromIntegral a `div` 4
        i2 = ((fromIntegral a `mod` 4) * 16) + (fromIntegral b `div` 16)
        i3 = ((fromIntegral b `mod` 16) * 4) + (fromIntegral c `div` 64)
        i4 = fromIntegral c `mod` 64
     in base64Char i1 : base64Char i2 : base64Char i3 : base64Char i4 : go rest

  base64Char :: Int -> Char
  base64Char n
    | n < 26 = toEnum (fromEnum 'A' + n)
    | n < 52 = toEnum (fromEnum 'a' + n - 26)
    | n < 62 = toEnum (fromEnum '0' + n - 52)
    | n == 62 = '+'
    | otherwise = '/'


-- | Escape XML special characters
escapeXML :: Text -> Text
escapeXML = T.concatMap escapeChar
 where
  escapeChar '<' = "&lt;"
  escapeChar '>' = "&gt;"
  escapeChar '&' = "&amp;"
  escapeChar '"' = "&quot;"
  escapeChar '\'' = "&apos;"
  escapeChar c = T.singleton c


-- | Show a floating point number with reasonable precision
showFloat :: Float -> String
showFloat f
  | isNaN f = "0"
  | isInfinite f = if f > 0 then "1e10" else "-1e10"
  | abs f < 0.0001 = "0"
  | abs (f - fromIntegral (round f :: Int)) < 0.0001 = show (round f :: Int)
  | otherwise = printf "%.4f" f


-- | Show a point
showPoint :: Point -> String
showPoint (x, y) = showFloat x ++ " " ++ showFloat y


-- | Generate opacity attribute if needed
opacityAttr :: Color -> Text
opacityAttr color =
  let a = colorOpacity color
   in if a < 1.0
        then T.pack $ printf " fill-opacity=\"%.3f\"" a
        else ""


-- | Generate stroke opacity attribute if needed
strokeOpacityAttr :: Color -> Text
strokeOpacityAttr color =
  let a = colorOpacity color
   in if a < 1.0
        then T.pack $ printf " stroke-opacity=\"%.3f\"" a
        else ""
