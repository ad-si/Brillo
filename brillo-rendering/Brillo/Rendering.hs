module Brillo.Rendering (
  -- * Picture data type
  Picture (..),
  Point,
  Vector,
  Path,

  -- * Colors
  Color,
  makeColor,
  makeColorI,
  makeRawColor,
  makeRawColorI,
  rgbaOfColor,
  clampColor,

  -- * Bitmaps
  Rectangle (..),
  BitmapData (..),
  BitmapFormat (..),
  PixelFormat (..),
  RowOrder (..),
  bitmapOfForeignPtr,
  bitmapDataOfForeignPtr,
  bitmapOfByteString,
  bitmapDataOfByteString,
  bitmapOfBMP,
  bitmapDataOfBMP,
  loadBMP,

  -- * Rendering
  displayPicture,
  renderPicture,
  withModelview,
  withClearBuffer,
  RS.initState,
  RS.State,
)
where

import Brillo.Internals.Data.Color
import Brillo.Internals.Data.Picture
import Brillo.Internals.Rendering.Common
import Brillo.Internals.Rendering.Picture
import Brillo.Internals.Rendering.State qualified as RS


{-| Set up the OpenGL context, clear the buffer, and render the given picture
  into it.

  This is the same as `renderPicture` composed with `withModelview`
  and `withClearBuffer`. If you want to manage your own OpenGL context then
  you can just call `renderPicture`.

  Using this function assumes that you've already opened a window
  and set that to the active context. If you don't want to do your own window
  management then use the @brillo@ package instead.
-}
displayPicture ::
  -- | Window width and height.
  (Int, Int) ->
  -- | Color to clear the window with.
  Color ->
  -- | Current rendering state.
  RS.State ->
  -- | View port scale, which controls the level of detail.
  --   Use 1.0 to start with.
  Float ->
  -- | Picture to draw.
  Picture ->
  IO ()
displayPicture windowSize colorClear state scale picture =
  withModelview windowSize $
    withClearBuffer colorClear $
      renderPicture state scale picture
