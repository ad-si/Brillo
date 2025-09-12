{-# LANGUAGE ExplicitForAll #-}

{-| This game mode lets you manage your own input. Pressing ESC will not abort the program.
  You also don't get automatic pan and zoom controls like with `display`.
-}
module Brillo.Interface.IO.Game (
  module Brillo.Data.Display,
  module Brillo.Data.Picture,
  module Brillo.Data.Color,
  playIO,
  Event (..),
  Key (..),
  SpecialKey (..),
  MouseButton (..),
  KeyState (..),
  Modifiers (..),
)
where

import Brillo.Data.Color
import Brillo.Data.Display
import Brillo.Data.Picture
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Game


-- | Play a game in a window, using IO actions to build the pictures.
playIO ::
  forall world.
  -- | Display mode.
  Display ->
  -- | Background color.
  Color ->
  -- | Number of simulation steps to take for each second of real time.
  Int ->
  -- | The initial world.
  world ->
  -- | An action to convert the world a picture.
  (world -> IO Picture) ->
  -- | A function to handle input events.
  (Event -> world -> IO world) ->
  -- | A function to step the world one iteration.
  --   It is passed the period of time (in seconds) needing to be advanced.
  (Float -> world -> IO world) ->
  IO ()
playIO
  display
  backColor
  simResolution
  worldStart
  worldToPicture
  worldHandleEvent
  worldAdvance =
    playWithBackendIO
      defaultBackendState
      display
      backColor
      simResolution
      worldStart
      worldToPicture
      worldHandleEvent
      worldAdvance
      False
