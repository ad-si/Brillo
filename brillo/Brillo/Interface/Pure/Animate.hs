
-- | Display mode is for drawing a static picture.
module Brillo.Interface.Pure.Animate
        ( module Brillo.Data.Display
        , module Brillo.Data.Picture
        , module Brillo.Data.Color
        , animate)
where
import Brillo.Data.Display
import Brillo.Data.Picture
import Brillo.Data.Color
import Brillo.Internals.Interface.Animate
import Brillo.Internals.Interface.Backend


-- | Open a new window and display the given animation.
--
--   Once the window is open you can use the same commands as with `display`.
--
animate :: Display              -- ^ Display mode.
        -> Color                -- ^ Background color.
        -> (Float -> Picture)   -- ^ Function to produce the next frame of animation.
                                --      It is passed the time in seconds since the program started.
        -> IO ()

animate display backColor frameFun
        = animateWithBackendIO
                defaultBackendState
                True            -- pannable
                display backColor
                (return . frameFun)
                (const (return ()))
