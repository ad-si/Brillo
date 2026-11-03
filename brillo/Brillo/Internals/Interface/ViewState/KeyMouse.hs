{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}

module Brillo.Internals.Interface.ViewState.KeyMouse (
  callback_viewState_keyMouse,
  callback_viewState_scroll,
)
where

import Brillo.Data.ViewState
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Event
import Data.IORef


{-| Callback to handle keyboard and mouse button events
     for controlling the 'ViewState'.
-}
callback_viewState_keyMouse ::
  IORef ViewState ->
  Callback
callback_viewState_keyMouse viewStateRef =
  KeyMouse (viewState_keyMouse viewStateRef)


viewState_keyMouse :: IORef ViewState -> KeyboardMouseCallback
viewState_keyMouse viewStateRef stateRef key keyState keyMods pos =
  do
    viewState <- readIORef viewStateRef
    ev <- keyMouseEvent stateRef key keyState keyMods pos
    case updateViewStateWithEventMaybe ev viewState of
      Nothing -> return ()
      Just viewState' ->
        do
          viewStateRef `writeIORef` viewState'
          postRedisplay stateRef


{-| Callback to handle scroll events (mouse wheel, trackpad)
     for controlling the 'ViewState'.
-}
callback_viewState_scroll ::
  IORef ViewState ->
  Callback
callback_viewState_scroll viewStateRef =
  Scroll (viewState_scroll viewStateRef)


viewState_scroll :: IORef ViewState -> ScrollCallback
viewState_scroll viewStateRef stateRef offset keyMods pos =
  do
    viewState <- readIORef viewStateRef
    ev <- scrollEvent stateRef offset keyMods pos
    case updateViewStateWithEventMaybe ev viewState of
      Nothing -> return ()
      Just viewState' ->
        do
          viewStateRef `writeIORef` viewState'
          postRedisplay stateRef
