-- | Functions to load bitmap data from various places.
module Brillo.Data.Bitmap (
  Rectangle (..),
  BitmapData (..),
  BitmapFormat (..),
  RowOrder (..),
  PixelFormat (..),
  CompressedFormat (..),
  bitmapOfForeignPtr,
  bitmapDataOfForeignPtr,
  bitmapOfByteString,
  bitmapDataOfByteString,
  bitmapOfBMP,
  bitmapDataOfBMP,
  compressedBitmapOfForeignPtr,
  compressedBitmapDataOfForeignPtr,
  compressedBitmapOfByteString,
  compressedBitmapDataOfByteString,
  -- Common compressed texture format constants
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
  loadBMP,
)
where

import Brillo.Rendering

