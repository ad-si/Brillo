{-# LANGUAGE RankNTypes #-}

module Brillo.Internals.Interface.Event (
  Event (..),
  keyMouseEvent,
  scrollEvent,
  motionEvent,
  dropEvent,
  pickEvent,
)
where

import Brillo.Internals.Interface.Backend
import Data.IORef


-- | Possible input events.
data Event
  = {-| A key or mouse button was pressed or released.
    Arguments: key, key state, modifiers, mouse position.
    -}
    EventKey Key KeyState Modifiers (Float, Float)
  | {-| The user scrolled with a mouse wheel or a trackpad.
    Arguments: scroll offset @(x, y)@, modifiers, mouse position.
    A positive y offset means scrolling up,
    a positive x offset means scrolling left.
    The directions are the ones reported by the operating system,
    so they are reversed if \"natural scrolling\" is enabled.
    One notch of a regular mouse wheel corresponds to an offset of @1@,
    while trackpads also report fractional offsets.
    -}
    EventScroll (Float, Float) Modifiers (Float, Float)
  | -- | The mouse was moved. Argument: mouse position.
    EventMotion (Float, Float)
  | EventResize (Int, Int)
  | EventDrop [FilePath]
  | EventPick [FilePath]
  deriving (Eq, Show)


keyMouseEvent ::
  forall a.
  (Backend a) =>
  IORef a ->
  Key ->
  KeyState ->
  Modifiers ->
  (Int, Int) ->
  IO Event
keyMouseEvent backendRef key keyState modifiers pos =
  EventKey key keyState modifiers <$> convertPoint backendRef pos


scrollEvent ::
  forall a.
  (Backend a) =>
  IORef a ->
  (Double, Double) ->
  Modifiers ->
  (Int, Int) ->
  IO Event
scrollEvent backendRef (offsetX, offsetY) modifiers pos =
  EventScroll (realToFrac offsetX, realToFrac offsetY) modifiers
    <$> convertPoint backendRef pos


motionEvent ::
  forall a.
  (Backend a) =>
  IORef a ->
  (Int, Int) ->
  IO Event
motionEvent backendRef pos =
  EventMotion <$> convertPoint backendRef pos


dropEvent ::
  forall a.
  (Backend a) =>
  IORef a ->
  [FilePath] ->
  IO Event
dropEvent _backendRef paths =
  return $ EventDrop paths


pickEvent ::
  forall a.
  (Backend a) =>
  IORef a ->
  [FilePath] ->
  IO Event
pickEvent _backendRef paths =
  return $ EventPick paths


convertPoint ::
  forall a.
  (Backend a) =>
  IORef a ->
  (Int, Int) ->
  IO (Float, Float)
convertPoint backendRef pos =
  do
    (sizeX_, sizeY_) <- getWindowDimensions backendRef
    let (sizeX, sizeY) = (fromIntegral sizeX_, fromIntegral sizeY_)

    let (px_, py_) = pos
    let px = fromIntegral px_
    let py = sizeY - fromIntegral py_

    let px' = px - sizeX / 2
    let py' = py - sizeY / 2
    let pos' = (px', py')
    return pos'
