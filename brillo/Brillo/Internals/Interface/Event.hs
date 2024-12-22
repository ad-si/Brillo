{-# LANGUAGE RankNTypes #-}

module Brillo.Internals.Interface.Event (
  Event (..),
  keyMouseEvent,
  motionEvent,
  dropEvent,
)
where

import Brillo.Internals.Interface.Backend
import Data.IORef


-- | Possible input events.
data Event
  = EventKey Key KeyState Modifiers (Float, Float)
  | EventMotion (Float, Float)
  | EventResize (Int, Int)
  | EventDrop [FilePath]
  deriving (Eq, Show)


keyMouseEvent
  :: forall a
   . (Backend a)
  => IORef a
  -> Key
  -> KeyState
  -> Modifiers
  -> (Int, Int)
  -> IO Event
keyMouseEvent backendRef key keyState modifiers pos =
  EventKey key keyState modifiers <$> convertPoint backendRef pos


motionEvent
  :: forall a
   . (Backend a)
  => IORef a
  -> (Int, Int)
  -> IO Event
motionEvent backendRef pos =
  EventMotion <$> convertPoint backendRef pos


dropEvent
  :: forall a
   . (Backend a)
  => IORef a
  -> [FilePath]
  -> IO Event
dropEvent _backendRef paths =
  return $ EventDrop paths

convertPoint
  :: forall a
   . (Backend a)
  => IORef a
  -> (Int, Int)
  -> IO (Float, Float)
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
