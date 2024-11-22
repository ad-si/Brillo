
-- | Animate a picture in a window.
module Brillo.Interface.IO.Animate
        ( module Brillo.Data.Display
        , module Brillo.Data.Picture
        , module Brillo.Data.Color
        , animateIO
        , animateFixedIO
        , Controller (..))
where
import Brillo.Data.Display
import Brillo.Data.Controller
import Brillo.Data.Picture
import Brillo.Data.Color
import Brillo.Internals.Interface.Animate
import Brillo.Internals.Interface.Backend


-- | Open a new window and display the given animation.
--
--   Once the window is open you can use the same commands as with @display@.
--
animateIO
        :: Display                -- ^ Display mode.
        -> Color                  -- ^ Background color.
        -> (Float -> IO Picture)  -- ^ Function to produce the next frame of animation.
                                  --      It is passed the time in seconds since the program started.
        -> (Controller -> IO ())  -- ^ Callback to take the display controller.
        -> IO ()

animateIO display backColor
        frameFunIO eatControllerIO
        = animateWithBackendIO
                defaultBackendState
                True              -- pannable
                display backColor
                frameFunIO
                eatControllerIO


-- | Like `animateIO` but don't allow the display to be panned around.
--
animateFixedIO
        :: Display                -- ^ Display mode.
        -> Color                  -- ^ Background color.
        -> (Float -> IO Picture)  -- ^ Function to produce the next frame of animation.
                                  --      It is passed the time in seconds since the program started.
        -> (Controller -> IO ())  -- ^ Callback to take the display controller.
        -> IO ()

animateFixedIO display backColor
        frameFunIO eatControllerIO
        = animateWithBackendIO
                defaultBackendState
                False
                display backColor
                frameFunIO
                eatControllerIO

