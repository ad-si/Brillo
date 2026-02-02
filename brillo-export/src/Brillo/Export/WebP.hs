module Brillo.Export.WebP (
  exportPictureToWebP,
  exportPicturesToWebP,
  exportPictureToWebPLossy,
  exportPicturesToWebPLossy,
) where

import qualified Brillo.Rendering as Brillo
import Codec.Picture.Types (Image, PixelRGBA8)
import qualified Codec.Picture.WebP as WebP
import qualified Data.ByteString as BS
import Foreign.C.Types (CFloat)

import Brillo.Export.Image


-- | Write a WebP file (lossless)
writeWebPLossless :: FilePath -> Image PixelRGBA8 -> IO ()
writeWebPLossless path img = BS.writeFile path (WebP.encodeRgba8Lossless img)


-- | Write a WebP file with lossy compression
writeWebPLossy :: CFloat -> FilePath -> Image PixelRGBA8 -> IO ()
writeWebPLossy quality path img = BS.writeFile path (WebP.encodeRgba8 quality img)


-- | Save a Brillo Picture as lossless WebP
exportPictureToWebP ::
  -- | width, height in pixels
  Size ->
  -- | background color
  Brillo.Color ->
  FilePath ->
  Brillo.Picture ->
  IO ()
exportPictureToWebP = exportPictureToFormat writeWebPLossless


-- | Save a Brillo animation as lossless WebP
exportPicturesToWebP ::
  -- | width, height in pixels
  Size ->
  -- | background color
  Brillo.Color ->
  -- | filepath pattern (must contain "%d" for frame number)
  FilePath ->
  -- | function that maps from point in time to Picture
  Animation ->
  -- | list of points in time at which to evaluate the animation
  [Float] ->
  IO ()
exportPicturesToWebP = exportPicturesToFormat writeWebPLossless


-- | Save a Brillo Picture as lossy WebP with specified quality
exportPictureToWebPLossy ::
  -- | quality (0.0 to 100.0)
  Float ->
  -- | width, height in pixels
  Size ->
  -- | background color
  Brillo.Color ->
  FilePath ->
  Brillo.Picture ->
  IO ()
exportPictureToWebPLossy quality =
  exportPictureToFormat (writeWebPLossy (realToFrac quality))


-- | Save a Brillo animation as lossy WebP with specified quality
exportPicturesToWebPLossy ::
  -- | quality (0.0 to 100.0)
  Float ->
  -- | width, height in pixels
  Size ->
  -- | background color
  Brillo.Color ->
  -- | filepath pattern (must contain "%d" for frame number)
  FilePath ->
  -- | function that maps from point in time to Picture
  Animation ->
  -- | list of points in time at which to evaluate the animation
  [Float] ->
  IO ()
exportPicturesToWebPLossy quality =
  exportPicturesToFormat (writeWebPLossy (realToFrac quality))
