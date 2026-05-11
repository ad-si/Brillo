{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Helper functions for rendering bitmaps
module Brillo.Internals.Rendering.Bitmap (
  Rectangle (..),
  BitmapData (..),
  BitmapFormat (..),
  PixelFormat (..),
  RowOrder (..),
  CompressedFormat (..),
  -- Common compressed format constants
  compressedRGB_S3TC_DXT1,
  compressedRGBA_S3TC_DXT1,
  compressedRGBA_S3TC_DXT3,
  compressedRGBA_S3TC_DXT5,
  compressedSRGB_S3TC_DXT1,
  compressedSRGB_ALPHA_S3TC_DXT1,
  compressedSRGB_ALPHA_S3TC_DXT3,
  compressedSRGB_ALPHA_S3TC_DXT5,
  compressedRED_RGTC1,
  compressedSIGNED_RED_RGTC1,
  compressedRG_RGTC2,
  compressedSIGNED_RG_RGTC2,
  compressedRGBA_BPTC_UNORM,
  compressedSRGB_ALPHA_BPTC_UNORM,
  compressedRGB_BPTC_SIGNED_FLOAT,
  compressedRGB_BPTC_UNSIGNED_FLOAT,
  bitmapPath,
  freeBitmapData,
)
where

import Data.Data (Data, Typeable)
import Data.Word (Word32)
import Foreign (ForeignPtr, Ptr, Word8, free)


-- | Represents a rectangular section in a bitmap
data Rectangle
  = Rectangle
  { rectPos :: (Int, Int)
  -- ^ x- and y-pos in the bitmap in pixels
  , rectSize :: (Int, Int)
  -- ^ width/height of the area in pixelsi
  }
  deriving (Show, Read, Eq, Ord, Data, Typeable)


-- | Abstract 32-bit RGBA bitmap data.
data BitmapData
  = BitmapData
  { bitmapDataLength :: Int -- length (in bytes)
  , bitmapFormat :: BitmapFormat
  , bitmapSize :: (Int, Int)
  -- ^ width, height in pixels
  , bitmapCacheMe :: Bool
  , bitmapPointer :: ForeignPtr Word8
  }
  deriving (Eq, Data, Typeable)


{-| Description of how the bitmap is layed out in memory.

  * Prior version of Brillo assumed `BitmapFormat BottomToTop PxABGR`
-}
data BitmapFormat
  = BitmapFormat
  { rowOrder :: RowOrder
  , pixelFormat :: PixelFormat
  }
  deriving (Eq, Data, Typeable, Show, Ord)


{-| Order of rows in an image are either:

  * `TopToBottom` - the top row, followed by the next-lower row and so on.
  * `BottomToTop` - the bottom row followed by the next-higher row and so on.
-}
data RowOrder
  = TopToBottom
  | BottomToTop
  deriving (Eq, Data, Typeable, Show, Ord, Enum, Bounded)


{-| Pixel formats describe how the texel data is laid out in memory.

  `PxRGBA` and `PxABGR` are uncompressed 32-bit-per-pixel formats.

  `PxCompressed` carries a hardware compressed format token that the GPU
  decodes on its own. The bitmap's `bitmapDataLength` field must hold the
  exact size in bytes of the compressed payload, since the size cannot be
  derived from width and height alone.
-}
data PixelFormat
  = PxRGBA
  | PxABGR
  | PxCompressed CompressedFormat
  deriving (Eq, Data, Typeable, Show, Ord)


{-| Identifier of a GPU-side compressed texture format.

  The wrapped value is the OpenGL @internalformat@ enum (for example
  @0x83F1@ for @GL_COMPRESSED_RGBA_S3TC_DXT1_EXT@). Use the constants
  exported alongside this type, or supply an enum from
  "Graphics.Rendering.OpenGL.Raw" directly.
-}
newtype CompressedFormat = CompressedFormat Word32
  deriving (Eq, Data, Typeable, Show, Ord)


-- S3TC / DXT --------------------------------------------------------------

compressedRGB_S3TC_DXT1 :: CompressedFormat
compressedRGB_S3TC_DXT1 = CompressedFormat 0x83F0


compressedRGBA_S3TC_DXT1 :: CompressedFormat
compressedRGBA_S3TC_DXT1 = CompressedFormat 0x83F1


compressedRGBA_S3TC_DXT3 :: CompressedFormat
compressedRGBA_S3TC_DXT3 = CompressedFormat 0x83F2


compressedRGBA_S3TC_DXT5 :: CompressedFormat
compressedRGBA_S3TC_DXT5 = CompressedFormat 0x83F3


compressedSRGB_S3TC_DXT1 :: CompressedFormat
compressedSRGB_S3TC_DXT1 = CompressedFormat 0x8C4C


compressedSRGB_ALPHA_S3TC_DXT1 :: CompressedFormat
compressedSRGB_ALPHA_S3TC_DXT1 = CompressedFormat 0x8C4D


compressedSRGB_ALPHA_S3TC_DXT3 :: CompressedFormat
compressedSRGB_ALPHA_S3TC_DXT3 = CompressedFormat 0x8C4E


compressedSRGB_ALPHA_S3TC_DXT5 :: CompressedFormat
compressedSRGB_ALPHA_S3TC_DXT5 = CompressedFormat 0x8C4F


-- RGTC --------------------------------------------------------------------

compressedRED_RGTC1 :: CompressedFormat
compressedRED_RGTC1 = CompressedFormat 0x8DBB


compressedSIGNED_RED_RGTC1 :: CompressedFormat
compressedSIGNED_RED_RGTC1 = CompressedFormat 0x8DBC


compressedRG_RGTC2 :: CompressedFormat
compressedRG_RGTC2 = CompressedFormat 0x8DBD


compressedSIGNED_RG_RGTC2 :: CompressedFormat
compressedSIGNED_RG_RGTC2 = CompressedFormat 0x8DBE


-- BPTC --------------------------------------------------------------------

compressedRGBA_BPTC_UNORM :: CompressedFormat
compressedRGBA_BPTC_UNORM = CompressedFormat 0x8E8C


compressedSRGB_ALPHA_BPTC_UNORM :: CompressedFormat
compressedSRGB_ALPHA_BPTC_UNORM = CompressedFormat 0x8E8D


compressedRGB_BPTC_SIGNED_FLOAT :: CompressedFormat
compressedRGB_BPTC_SIGNED_FLOAT = CompressedFormat 0x8E8E


compressedRGB_BPTC_UNSIGNED_FLOAT :: CompressedFormat
compressedRGB_BPTC_UNSIGNED_FLOAT = CompressedFormat 0x8E8F


instance Show BitmapData where
  show _ = "BitmapData"


-- | Generates the point path to display the bitmap centred
bitmapPath :: Float -> Float -> [(Float, Float)]
bitmapPath width height =
  [ (-width', -height')
  , (width', -height')
  , (width', height')
  , (-width', height')
  ]
  where
    width' = width / 2
    height' = height / 2


-- | Frees the allocated memory given to OpenGL to avoid a memory leak
freeBitmapData :: Ptr Word8 -> IO ()
freeBitmapData = free
{-# INLINE freeBitmapData #-}
