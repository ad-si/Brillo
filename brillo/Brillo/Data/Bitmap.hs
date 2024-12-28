-- | Functions to load bitmap data from various places.
module Brillo.Data.Bitmap (
  Rectangle (..),
  BitmapData (..),
  BitmapFormat (..),
  RowOrder (..),
  PixelFormat (..),
  bitmapOfForeignPtr,
  bitmapDataOfForeignPtr,
  bitmapOfByteString,
  bitmapDataOfByteString,
  bitmapOfBMP,
  bitmapDataOfBMP,
  loadBMP,
)
where

import Brillo.Rendering

