module Brillo.Export.Tga (
  exportPictureToTga,
  exportPicturesToTga,
) where

import qualified Brillo.Rendering as Brillo
import Codec.Picture.Tga (writeTga)

import Brillo.Export.Image


-- | Save a Brillo Picture as Tga
exportPictureToTga ::
  -- | width, height in pixels
  Size ->
  -- | background color
  Brillo.Color ->
  FilePath ->
  Brillo.Picture ->
  IO ()
exportPictureToTga = exportPictureToFormat writeTga


-- | Save a Brillo animation as Tga
exportPicturesToTga ::
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
exportPicturesToTga = exportPicturesToFormat writeTga
