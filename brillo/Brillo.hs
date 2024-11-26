{-| Brillo hides the pain of drawing simple vector graphics behind a nice data type and
     a few display functions.

  Getting something on the screen is as easy as:

 @
 import Brillo
 main = `display` (InWindow \"Nice Window\" (200, 200) (10, 10)) `white` (`Circle` 80)
 @

  Once the window is open you can use the following:

@
* Quit
  - esc-key

* Move Viewport
  - arrow keys
  - left-click drag

* Zoom Viewport
  - page up/down-keys
  - control-left-click drag
  - right-click drag
  - mouse wheel

* Rotate Viewport
  - home/end-keys
  - alt-left-click drag

* Reset Viewport
  'r'-key
@


  Animations can be constructed similarly using the `animate`.

  If you want to run a simulation based around finite time steps then try
  `simulate`.

  If you want to manage your own key\/mouse events then use `play`.

  Brillo uses OpenGL under the hood, but you don't have to worry about any of that.

  Brillo programs should be compiled with @-threaded@, otherwise the GHC runtime
  will limit the frame-rate to around 20Hz.


For more information, check out <https://github.com/ad-si/Brillo>.
-}
module Brillo (
  module Brillo.Data.Picture,
  module Brillo.Data.Color,
  module Brillo.Data.Bitmap,
  Display (..),
  display,
  animate,
  simulate,
  play,
)
where

import Brillo.Data.Bitmap
import Brillo.Data.Color
import Brillo.Data.Display
import Brillo.Data.Picture
import Brillo.Interface.Pure.Animate
import Brillo.Interface.Pure.Display
import Brillo.Interface.Pure.Game
import Brillo.Interface.Pure.Simulate

