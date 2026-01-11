module Brillo.Export.Tiff (
  exportPictureToTiff,
  exportPicturesToTiff,
) where

import qualified Brillo.Rendering as Brillo
import Codec.Picture.Tiff (writeTiff)

import Brillo.Export.Image


-- | Save a Brillo Picture as Tiff
exportPictureToTiff ::
  -- | width, height in pixels
  Size ->
  -- | background color
  Brillo.Color ->
  FilePath ->
  Brillo.Picture ->
  IO ()
exportPictureToTiff = exportPictureToFormat writeTiff


-- | Save a Brillo animation as Tiff
exportPicturesToTiff ::
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
exportPicturesToTiff = exportPicturesToFormat writeTiff
