{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Data types for representing pictures.
module Brillo.Internals.Data.Picture (
  Point,
  Vector,
  Path,
  Picture (..),

  -- * Bitmaps
  Rectangle (..),
  BitmapData (..),
  PixelFormat (..),
  BitmapFormat (..),
  RowOrder (..),
  bitmapOfForeignPtr,
  bitmapDataOfForeignPtr,
  bitmapOfByteString,
  bitmapDataOfByteString,
  bitmapOfBMP,
  bitmapDataOfBMP,
  loadBMP,
  rectAtOrigin,
)
where

import Brillo.Internals.Data.Color (Color)
import Brillo.Internals.Rendering.Bitmap (
  BitmapData (..),
  BitmapFormat (..),
  PixelFormat (..),
  Rectangle (..),
  RowOrder (..),
 )
import Codec.BMP (BMP, bmpDimensions, readBMP, unpackBMPToRGBA32)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as BSU
import Data.Data (Data, Typeable)
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (map)


#if __GLASGOW_HASKELL__ >= 800
import Data.Semigroup
import Data.List.NonEmpty
#endif


-- | A point on the x-y plane.
type Point = (Float, Float)


-- | A vector can be treated as a point, and vis-versa.
type Vector = Point


-- | A path through the x-y plane.
type Path = [Point]


-- | A 2D picture
data Picture
  = -- Primitives -------------------------------------

    -- | A blank picture, with nothing in it.
    Blank
  | -- | A convex polygon filled with a solid color.
    Polygon Path
  | -- | A line along an arbitrary path.
    Line Path
  | -- | A smooth line along an arbitrary path.
    LineSmooth Path
  | -- | A line along an arbitrary path with a given thickness.
    ThickLine Path Float
  | -- | A smooth line along an arbitrary path with a given thickness.
    ThickLineSmooth Path Float
  | -- | A circle with the given radius.
    Circle Float
  | -- | A circle with the given radius and thickness.
    --   If the thickness is 0 then this is equivalent to `Circle`.
    ThickCircle Float Float
  | -- | A circular arc drawn counter-clockwise between two angles
    --  (in degrees) at the given radius.
    Arc Float Float Float
  | -- | A circular arc drawn counter-clockwise between two angles
    --  (in degrees), with the given radius and thickness.
    --   If the thickness is 0 then this is equivalent to `Arc`.
    ThickArc Float Float Float Float
  | -- | Text to draw with a vector font
    Text Text
  | -- | Text to draw with a vector font and a given thickness.
    ThickText Text Float
  | -- | A bitmap image.
    Bitmap BitmapData
  | -- | A subsection of a bitmap image where
    --   the first argument selects a sub section in the bitmap,
    --   and second argument determines the bitmap data.
    BitmapSection Rectangle BitmapData
  | -- Color ------------------------------------------

    -- | A picture drawn with this color.
    Color Color Picture
  | -- Transforms -------------------------------------

    -- | A picture translated by the given x and y coordinates.
    Translate Float Float Picture
  | -- | A picture rotated clockwise by the given angle (in degrees).
    Rotate Float Picture
  | -- | A picture scaled by the given x and y factors.
    Scale Float Float Picture
  | -- More Pictures ----------------------------------

    -- | A picture consisting of several others.
    Pictures [Picture]
  deriving (Show, Eq, Data, Typeable)


-- Instances ------------------------------------------------------------------
instance Monoid Picture where
  mempty = Blank
  mconcat = Pictures

#if __GLASGOW_HASKELL__ >= 800
instance Semigroup Picture where
  a <> b          = Pictures [a, b]
  sconcat         = Pictures . toList
  stimes          = stimesIdempotent
#endif


-- Bitmaps --------------------------------------------------------------------

{-| O(1). Use a `ForeignPtr` of RGBA data as a bitmap with the given
  width and height.

  The boolean flag controls whether Brillo should cache the data
  between frames for speed. If you are programatically generating
  the image for each frame then use `False`. If you have loaded it
  from a file then use `True`.
-}
bitmapOfForeignPtr :: Int -> Int -> BitmapFormat -> ForeignPtr Word8 -> Bool -> Picture
bitmapOfForeignPtr width height fmt fptr cacheMe =
  Bitmap $
    bitmapDataOfForeignPtr width height fmt fptr cacheMe


-- Bitmap width height (bitmapDataOfForeignPtr width height fmt fptr) cacheMe

bitmapDataOfForeignPtr :: Int -> Int -> BitmapFormat -> ForeignPtr Word8 -> Bool -> BitmapData
bitmapDataOfForeignPtr width height fmt fptr cacheMe =
  let len = width * height * 4
  in  BitmapData len fmt (width, height) cacheMe fptr


{-| O(size). Copy a `ByteString` of RGBA data into a bitmap with the given
  width and height.

  The boolean flag controls whether Brillo should cache the data
  between frames for speed. If you are programatically generating
  the image for each frame then use `False`. If you have loaded it
  from a file then use `True`.
-}
bitmapOfByteString :: Int -> Int -> BitmapFormat -> ByteString -> Bool -> Picture
bitmapOfByteString width height fmt bs cacheMe =
  Bitmap $
    bitmapDataOfByteString width height fmt bs cacheMe


bitmapDataOfByteString :: Int -> Int -> BitmapFormat -> ByteString -> Bool -> BitmapData
bitmapDataOfByteString width height fmt bs cacheMe =
  unsafePerformIO $
    do
      let len = width * height * 4
      ptr <- mallocBytes len
      fptr <- newForeignPtr finalizerFree ptr

      BSU.unsafeUseAsCString bs $
        \cstr -> copyBytes ptr (castPtr cstr) len

      return $ BitmapData len fmt (width, height) cacheMe fptr
{-# NOINLINE bitmapDataOfByteString #-}


-- | O(size). Copy a `BMP` file into a bitmap.
bitmapOfBMP :: BMP -> Picture
bitmapOfBMP bmp =
  Bitmap $ bitmapDataOfBMP bmp


-- | O(size). Copy a `BMP` file into a bitmap.
bitmapDataOfBMP :: BMP -> BitmapData
bitmapDataOfBMP bmp =
  unsafePerformIO $
    do
      let (width, height) = bmpDimensions bmp
      let bs = unpackBMPToRGBA32 bmp
      let len = width * height * 4

      ptr <- mallocBytes len
      fptr <- newForeignPtr finalizerFree ptr

      BSU.unsafeUseAsCString bs $
        \cstr -> copyBytes ptr (castPtr cstr) len

      return $ BitmapData len (BitmapFormat BottomToTop PxRGBA) (width, height) True fptr
{-# NOINLINE bitmapDataOfBMP #-}


-- | Load an uncompressed 24 or 32bit RGBA BMP file as a bitmap.
loadBMP :: FilePath -> IO Picture
loadBMP filePath =
  do
    ebmp <- readBMP filePath
    case ebmp of
      Left err -> error $ show err
      Right bmp -> return $ bitmapOfBMP bmp


{-| Construct a rectangle of the given width and height,
  with the lower left corner at the origin.
-}
rectAtOrigin :: Int -> Int -> Rectangle
rectAtOrigin w h = Rectangle (0, 0) (w, h)
