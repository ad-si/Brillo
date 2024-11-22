{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE RankNTypes #-}

module Brillo.Internals.Interface.ViewState.KeyMouse
        (callback_viewState_keyMouse)
where
import Brillo.Data.ViewState
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Event
import Data.IORef


-- | Callback to handle keyboard and mouse button events
--      for controlling the 'ViewState'.
callback_viewState_keyMouse
        :: IORef ViewState
        -> Callback

callback_viewState_keyMouse viewStateRef
        = KeyMouse (viewState_keyMouse viewStateRef)


viewState_keyMouse :: IORef ViewState -> KeyboardMouseCallback
viewState_keyMouse viewStateRef stateRef key keyState keyMods pos
 = do   viewState <- readIORef viewStateRef
        ev        <- keyMouseEvent stateRef key keyState keyMods pos
        case updateViewStateWithEventMaybe ev viewState of
                Nothing -> return ()
                Just viewState'
                 -> do  viewStateRef `writeIORef` viewState'
                        postRedisplay stateRef

