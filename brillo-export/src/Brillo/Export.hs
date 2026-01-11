{-| Main module for exporting Brillo pictures to image files, gif animations and juicy-pixels image datatypes.
  While no screen is displayed during the export process, the canvas is still limited to the screen's
  resolution.
-}
module Brillo.Export (
  -- * For tinkering yourself
  withBrilloState,
  withImage,
  withImages,
  exportPictureToFormat,
  exportPicturesToFormat,

  -- * Writing to PNG
  exportPictureToPNG,
  exportPicturesToPNG,

  -- * Writing to Bitmap
  exportPictureToBitmap,
  exportPicturesToBitmap,

  -- * Writing to Tga
  exportPictureToTga,
  exportPicturesToTga,

  -- * Writing to Tiff
  exportPictureToTiff,
  exportPicturesToTiff,

  -- * Writing to Gif
  exportPicturesToGif,
  GifDelay (..),
  GifLooping (..),
) where

import Brillo.Export.Bitmap
import Brillo.Export.Gif
import Brillo.Export.Image
import Brillo.Export.PNG
import Brillo.Export.Tga
import Brillo.Export.Tiff

