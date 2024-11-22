
-- | Display mode is for drawing a static picture.
module Brillo.Interface.IO.Interact
        ( module Brillo.Data.Display
        , module Brillo.Data.Picture
        , module Brillo.Data.Color
        , interactIO
        , Controller    (..)
        , Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..))
where
import Brillo.Data.Display
import Brillo.Data.Controller
import Brillo.Data.Picture
import Brillo.Data.Color
import Brillo.Internals.Interface.Event
import Brillo.Internals.Interface.Interact
import Brillo.Internals.Interface.Backend


-- | Open a new window and interact with an infrequently updated picture.
--
--   Similar to `displayIO`, except that you manage your own events.
--
interactIO
        :: Display                      -- ^ Display mode.
        -> Color                        -- ^ Background color.
        -> world                        -- ^ Initial world state.
        -> (world -> IO Picture)        -- ^ A function to produce the current picture.
        -> (Event -> world -> IO world) -- ^ A function to handle input events.
        -> (Controller -> IO ())        -- ^ Callback to take the display controller.
        -> IO ()

interactIO dis backColor worldInit makePicture handleEvent eatController
 =      interactWithBackend
                defaultBackendState
                dis
                backColor
                worldInit
                makePicture
                handleEvent
                eatController
