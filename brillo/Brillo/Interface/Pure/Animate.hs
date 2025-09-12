-- | Display mode is for drawing a static picture.
module Brillo.Interface.Pure.Animate (
  module Brillo.Data.Display,
  module Brillo.Data.Picture,
  module Brillo.Data.Color,
  animate,
)
where

import Brillo.Data.Color
import Brillo.Data.Display
import Brillo.Data.Picture
import Brillo.Internals.Interface.Animate
import Brillo.Internals.Interface.Backend


{-| Open a new window and display the given animation.

  Once the window is open you can use the same commands as with `display`.
-}
animate ::
  -- | Display mode.
  Display ->
  -- | Background color.
  Color ->
  -- | Function to produce the next frame of animation.
  --      It is passed the time in seconds since the program started.
  (Float -> Picture) ->
  IO ()
animate display backColor frameFun =
  animateWithBackendIO
    defaultBackendState
    True -- pannable
    display
    backColor
    (return . frameFun)
    (const (return ()))
