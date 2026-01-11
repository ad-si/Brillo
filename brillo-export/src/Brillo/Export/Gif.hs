module Brillo.Export.Gif (
  exportPicturesToGif,
  GifDelay (..),
  GifLooping (..),
) where

-- import Codec.Picture.Types

import qualified Brillo.Rendering as Brillo
import Codec.Picture

import Brillo.Export.Image


-- | Save a Brillo animation as PNG
exportPicturesToGif ::
  -- | time between frames in centiseconds
  GifDelay ->
  GifLooping ->
  -- | width, height in pixels as in Brillo.Display
  Size ->
  -- | background color
  Brillo.Color ->
  FilePath ->
  -- | function that maps from point in time to Picture. analog to Brillo.Animation
  Animation ->
  -- | list of points in time at which to evaluate the animation
  [Float] ->
  IO ()
exportPicturesToGif gifDelay gifLooping size bgc f anim ts = do
  withBrilloState size $ \s -> do
    withImages size bgc s (map anim ts) $ \imgs -> do
      case writeGifAnimation f gifDelay gifLooping imgs of
        Left errmsg -> error errmsg
        Right io -> io

{- slower on my machine -> not distributed
   but maybe helpful in the future e.g. when juicy-pixels offers `writeGifAnimation` with RGBA8
exportPicturesToGifByConverting :: GifDelay    -- ^ time between frames (Int)
                    -> GifLooping
                    -> Size        -- ^ width, height in pixels
                    -> Brillo.Color -- ^ background color
                    -> FilePath
                    -> Animation   -- ^ function that maps from point in time to Picture. analog to Brillo.Animation
                    -> [Float]     -- ^ list of points in time at which to evaluate the animation
                    -> IO ()
exportPicturesToGifByConverting gifDelay gifLooping size bgc f anim ts = do
  s <- initialize size
  (imgs, ptrs) <- mapAndUnzipM (pictureToImageRGBA8 size bgc s) (map anim ts)
  let imgs' = map (convertRGB8 . ImageRGBA8) imgs
  case writeGifAnimation f gifDelay gifLooping imgs' of
    Left errmsg -> putStrLn ("Error:" ++ errmsg)
    Right io -> io
  forM_ ptrs $ \ptr -> do
    free ptr
-}

{- RGB8
real	0m2,229s
user	0m4,030s
sys	0m1,874s

RGBA8 then conversion
real	0m3,438s
user	0m6,987s
sys	0m2,649s

-}
