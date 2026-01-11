module Brillo.Export.PNG (
  exportPictureToPNG,
  exportPicturesToPNG,
) where

import qualified Brillo.Rendering as Brillo
import Codec.Picture.Png (writePng)

import Brillo.Export.Image


-- | Save a Brillo Picture as PNG
exportPictureToPNG ::
  -- | width, height in pixels
  Size ->
  -- | background color
  Brillo.Color ->
  FilePath ->
  Brillo.Picture ->
  IO ()
exportPictureToPNG = exportPictureToFormat writePng


-- | Save a Brillo animation as PNG
exportPicturesToPNG ::
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
exportPicturesToPNG = exportPicturesToFormat writePng
