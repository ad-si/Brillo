{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}

module Brillo.Internals.Interface.ViewState.Motion (callback_viewState_motion)
where

import Brillo.Data.ViewState
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Event
import Data.IORef


{-| Callback to handle keyboard and mouse button events
     for controlling the viewport.
-}
callback_viewState_motion
  :: IORef ViewState
  -> Callback
callback_viewState_motion portRef =
  Motion (viewState_motion portRef)


viewState_motion :: IORef ViewState -> MotionCallback
viewState_motion viewStateRef stateRef pos =
  do
    viewState <- readIORef viewStateRef
    ev <- motionEvent stateRef pos
    case updateViewStateWithEventMaybe ev viewState of
      Nothing -> return ()
      Just viewState' ->
        do
          viewStateRef `writeIORef` viewState'
          postRedisplay stateRef
