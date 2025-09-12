-- | Display mode is for drawing a static picture.
module Brillo.Interface.IO.Interact (
  module Brillo.Data.Display,
  module Brillo.Data.Picture,
  module Brillo.Data.Color,
  interactIO,
  Controller (..),
  Event (..),
  Key (..),
  SpecialKey (..),
  MouseButton (..),
  KeyState (..),
  Modifiers (..),
)
where

import Brillo.Data.Color
import Brillo.Data.Controller
import Brillo.Data.Display
import Brillo.Data.Picture
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Event
import Brillo.Internals.Interface.Interact


{-| Open a new window and interact with an infrequently updated picture.

  Similar to `displayIO`, except that you manage your own events.
-}
interactIO ::
  -- | Display mode.
  Display ->
  -- | Background color.
  Color ->
  -- | Initial world state.
  world ->
  -- | A function to produce the current picture.
  (world -> IO Picture) ->
  -- | A function to handle input events.
  (Event -> world -> IO world) ->
  -- | Callback to take the display controller.
  (Controller -> IO ()) ->
  IO ()
interactIO dis backColor worldInit makePicture handleEvent eatController =
  interactWithBackend
    defaultBackendState
    dis
    backColor
    worldInit
    makePicture
    handleEvent
    eatController
