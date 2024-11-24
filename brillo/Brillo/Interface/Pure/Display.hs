-- | Display mode is for drawing a static picture.
module Brillo.Interface.Pure.Display (
  module Brillo.Data.Display,
  module Brillo.Data.Picture,
  module Brillo.Data.Color,
  display,
)
where

import Brillo.Data.Color
import Brillo.Data.Display
import Brillo.Data.Picture
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Display


-- | Open a new window and display the given picture.
display
  :: Display
  -- ^ Display mode.
  -> Color
  -- ^ Background color.
  -> Picture
  -- ^ The picture to draw.
  -> IO ()
display dis backColor picture =
  displayWithBackend
    defaultBackendState
    dis
    backColor
    (return picture)
    (const (return ()))
