module Brillo.Export.Bitmap (
  exportPictureToBitmap,
  exportPicturesToBitmap,
) where

import qualified Brillo.Rendering as Brillo
import Codec.Picture.Bitmap (writeBitmap)

import Brillo.Export.Image


-- | Save a Brillo Picture as Bitmap
exportPictureToBitmap ::
  -- | width, height in pixels
  Size ->
  -- | background color
  Brillo.Color ->
  FilePath ->
  Brillo.Picture ->
  IO ()
exportPictureToBitmap = exportPictureToFormat writeBitmap


-- | Save a Brillo animation as Bitmap
exportPicturesToBitmap ::
  -- | width, height in pixels
  Size ->
  -- | background color
  Brillo.Color ->
  FilePath ->
  -- | function that maps from point in time to Picture. analog to Brillo.Animation
  Animation ->
  -- | list of points in time at which to evaluate the animation
  [Float] ->
  IO ()
exportPicturesToBitmap = exportPicturesToFormat writeBitmap
