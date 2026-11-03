{-# LANGUAGE PatternGuards #-}

module Brillo.Data.ViewState (
  Command (..),
  CommandKey (..),
  WheelDirection (..),
  CommandConfig,
  defaultCommandConfig,
  ViewState (..),
  viewStateInit,
  viewStateInitWithConfig,
  updateViewStateWithEvent,
  updateViewStateWithEventMaybe,
)
where

import Brillo.Data.Point.Arithmetic qualified as Pt
import Brillo.Data.Vector
import Brillo.Data.ViewPort
import Brillo.Geometry.Angle
import Brillo.Internals.Interface.Backend
import Brillo.Internals.Interface.Event
import Control.Monad (mplus)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe


-- | The commands suported by the view controller.
data Command
  = CRestore
  | CTranslate
  | CRotate
  | CScale
  | -- bump zoom
    CBumpZoomOut
  | CBumpZoomIn
  | -- bump translate
    CBumpLeft
  | CBumpRight
  | CBumpUp
  | CBumpDown
  | -- bump rotate
    CBumpClockwise
  | CBumpCClockwise
  deriving (Show, Eq, Ord)


-- | An input that can trigger a viewport command.
data CommandKey
  = -- | A key or mouse button press.
    CKey Key
  | {-| Scrolling with a mouse wheel or a trackpad.
    Only instant commands (i.e. not 'CTranslate', 'CRotate', and 'CScale')
    can be triggered by scrolling.
    They are applied proportionally to the scroll offset,
    and zooming keeps the point under the cursor fixed.
    -}
    CMouseWheel WheelDirection
  deriving (Show, Eq, Ord)


{-| Direction of a scroll.
  The direction is the one reported by the operating system,
  so it is reversed if \"natural scrolling\" is enabled.
-}
data WheelDirection
  = -- | Scrolling up.
    WheelUp
  | -- | Scrolling down.
    WheelDown
  | -- | Scrolling left.
    WheelLeft
  | -- | Scrolling right.
    WheelRight
  deriving (Show, Eq, Ord)


type CommandConfig = [(Command, [(CommandKey, Maybe Modifiers)])]


{-| The default commands.  Left click pans, wheel zooms, right click
  rotates, "r" key resets.
-}
defaultCommandConfig :: CommandConfig
defaultCommandConfig =
  [
    ( CRestore
    , [(CKey $ Char 'r', Nothing)]
    )
  ,
    ( CTranslate
    ,
      [
        ( CKey $ MouseButton LeftButton
        , Just (Modifiers{shift = Up, ctrl = Up, alt = Up})
        )
      ]
    )
  ,
    ( CScale
    ,
      [
        ( CKey $ MouseButton LeftButton
        , Just (Modifiers{shift = Up, ctrl = Down, alt = Up})
        )
      ,
        ( CKey $ MouseButton RightButton
        , Just (Modifiers{shift = Up, ctrl = Up, alt = Up})
        )
      ]
    )
  ,
    ( CRotate
    ,
      [
        ( CKey $ MouseButton LeftButton
        , Just (Modifiers{shift = Up, ctrl = Up, alt = Down})
        )
      ,
        ( CKey $ MouseButton RightButton
        , Just (Modifiers{shift = Up, ctrl = Down, alt = Up})
        )
      ]
    )
  , -- bump zoom

    ( CBumpZoomOut
    ,
      [ (CMouseWheel WheelDown, Nothing)
      , (CKey $ SpecialKey KeyPageDown, Nothing)
      ]
    )
  ,
    ( CBumpZoomIn
    ,
      [ (CMouseWheel WheelUp, Nothing)
      , (CKey $ SpecialKey KeyPageUp, Nothing)
      ]
    )
  , -- bump translate

    ( CBumpLeft
    , [(CKey $ SpecialKey KeyLeft, Nothing)]
    )
  ,
    ( CBumpRight
    , [(CKey $ SpecialKey KeyRight, Nothing)]
    )
  ,
    ( CBumpUp
    , [(CKey $ SpecialKey KeyUp, Nothing)]
    )
  ,
    ( CBumpDown
    , [(CKey $ SpecialKey KeyDown, Nothing)]
    )
  , -- bump rotate

    ( CBumpClockwise
    , [(CKey $ SpecialKey KeyHome, Nothing)]
    )
  ,
    ( CBumpCClockwise
    , [(CKey $ SpecialKey KeyEnd, Nothing)]
    )
  ]


-- | Check if the provided key combination is some brillo viewport command.
isCommand ::
  Map Command [(CommandKey, Maybe Modifiers)] ->
  Command ->
  CommandKey ->
  Modifiers ->
  Bool
isCommand commands c key keyMods
  | Just csMatch <- Map.lookup c commands =
      or $ map (isCommand2 c key keyMods) csMatch
  | otherwise =
      False


-- | Check if the provided key combination is some brillo viewport command.
isCommand2 ::
  Command -> CommandKey -> Modifiers -> (CommandKey, Maybe Modifiers) -> Bool
isCommand2 _ key keyMods cMatch
  | (keyC, mModsC) <- cMatch
  , keyC == key
  , case mModsC of
      Nothing -> True
      Just modsC -> modsC == keyMods =
      True
  | otherwise =
      False


-- ViewControl State -----------------------------------------------------------

{-| State for controlling the viewport.
     These are used by the viewport control component.
-}
data ViewState
  = ViewState
  { viewStateCommands :: !(Map Command [(CommandKey, Maybe Modifiers)])
  {- ^ The command list for the viewport controller.
     These can be safely overwridden at any time by deleting
     or adding entries to the list.
     Entries at the front of the list take precedence.
  -}
  , viewStateScaleStep :: !Float
  {- ^ How much to scale the world by for each step of the mouse wheel.
     Fractional scroll offsets (e.g. from trackpads) scale the world
     by the corresponding power of this value.
  -}
  , viewStateRotateFactor :: !Float
  -- ^ How many degrees to rotate the world by for each pixel of x motion.
  , viewStateScaleFactor :: !Float
  -- ^ Ratio to scale the world by for each pixel of y motion.
  , viewStateTranslateMark :: !(Maybe (Float, Float))
  {- ^ During viewport translation,
     where the mouse was clicked on the window to start the translate.
  -}
  , viewStateRotateMark :: !(Maybe (Float, Float))
  {- ^ During viewport rotation,
     where the mouse was clicked on the window to starte the rotate.
  -}
  , viewStateScaleMark :: !(Maybe (Float, Float))
  {- ^ During viewport scale,
     where the mouse was clicked on the window to start the scale.
  -}
  , viewStateViewPort :: ViewPort
  -- ^ The current viewport.
  }


-- | The initial view state.
viewStateInit :: ViewState
viewStateInit =
  viewStateInitWithConfig defaultCommandConfig


-- | Initial view state, with user defined config.
viewStateInitWithConfig :: CommandConfig -> ViewState
viewStateInitWithConfig commandConfig =
  ViewState
    { viewStateCommands = Map.fromList commandConfig
    , viewStateScaleStep = 0.85
    , viewStateRotateFactor = 0.6
    , viewStateScaleFactor = 0.01
    , viewStateTranslateMark = Nothing
    , viewStateRotateMark = Nothing
    , viewStateScaleMark = Nothing
    , viewStateViewPort = viewPortInit
    }


-- | Apply an event to a `ViewState`.
updateViewStateWithEvent :: Event -> ViewState -> ViewState
updateViewStateWithEvent ev viewState =
  fromMaybe viewState $ updateViewStateWithEventMaybe ev viewState


{-| Like 'updateViewStateWithEvent', but returns 'Nothing' if no update
  was needed.
-}
updateViewStateWithEventMaybe :: Event -> ViewState -> Maybe ViewState
updateViewStateWithEventMaybe ev viewState =
  case ev of
    EventKey key keyState keyMods pos ->
      updateViewStateWithKeyMaybe key keyState keyMods pos viewState
    EventScroll offset keyMods pos ->
      updateViewStateWithScrollMaybe offset keyMods pos viewState
    -- Note that only a translation or rotation applies,
    -- not both at the same time.
    EventMotion pos ->
      motionScale (viewStateScaleMark viewState) pos viewState
        `mplus` motionTranslate (viewStateTranslateMark viewState) pos viewState
        `mplus` motionRotate (viewStateRotateMark viewState) pos viewState
    EventResize _ -> Nothing
    EventDrop _ -> Nothing
    EventPick _ -> Nothing


updateViewStateWithKeyMaybe ::
  Key -> KeyState -> Modifiers -> (Float, Float) -> ViewState -> Maybe ViewState
updateViewStateWithKeyMaybe key keyState keyMods pos viewState
  -- Instant commands, zooming around the center of the window.
  | keyState == Down
  , Just viewState' <- runInstantCommands (CKey key) keyMods 1 Nothing viewState =
      Just viewState'
  -- Start Translation.
  | isCommand' CTranslate
  , keyState == Down
  , not $ currentlyRotating || currentlyScaling =
      Just $ viewState{viewStateTranslateMark = Just pos}
  -- Start Rotation.
  | isCommand' CRotate
  , keyState == Down
  , not $ currentlyTranslating || currentlyScaling =
      Just $ viewState{viewStateRotateMark = Just pos}
  -- Start Scale.
  | isCommand' CScale
  , keyState == Down
  , not $ currentlyTranslating || currentlyRotating =
      Just $ viewState{viewStateScaleMark = Just pos}
  -- Kill current translate/rotate/scale command when the mouse button
  -- is released.
  | keyState == Up =
      let killTranslate vs = vs{viewStateTranslateMark = Nothing}
          killRotate vs = vs{viewStateRotateMark = Nothing}
          killScale vs = vs{viewStateScaleMark = Nothing}
      in  Just $
            (if currentlyTranslating then killTranslate else id) $
              (if currentlyRotating then killRotate else id) $
                (if currentlyScaling then killScale else id) $
                  viewState
  | otherwise =
      Nothing
  where
    isCommand' c = isCommand (viewStateCommands viewState) c (CKey key) keyMods
    currentlyTranslating = isJust $ viewStateTranslateMark viewState
    currentlyRotating = isJust $ viewStateRotateMark viewState
    currentlyScaling = isJust $ viewStateScaleMark viewState


{-| Run the instant commands bound to the scroll directions of the offset.
  Each axis is handled separately and runs its command
  as many times as the absolute offset says,
  so that fractional trackpad offsets result in fractional steps.
  Zooming keeps the point under the cursor fixed.
-}
updateViewStateWithScrollMaybe ::
  (Float, Float) -> Modifiers -> (Float, Float) -> ViewState -> Maybe ViewState
updateViewStateWithScrollMaybe (offsetX, offsetY) keyMods pos viewState =
  case scrollAxis offsetX WheelLeft WheelRight viewState of
    Nothing -> scrollAxis offsetY WheelUp WheelDown viewState
    Just viewState' ->
      Just $ fromMaybe viewState' $ scrollAxis offsetY WheelUp WheelDown viewState'
  where
    scrollAxis offset positiveDir negativeDir vs
      | offset == 0 = Nothing
      | otherwise =
          runInstantCommands
            (CMouseWheel $ if offset > 0 then positiveDir else negativeDir)
            keyMods
            (abs offset)
            (Just pos)
            vs


{-| Run the first instant (i.e. non-dragging) command
  that is bound to the given key and modifiers.
-}
runInstantCommands ::
  CommandKey ->
  Modifiers ->
  -- | Number of steps to apply the command for.
  Float ->
  -- | Point to zoom around. The center of the window if 'Nothing'.
  Maybe (Float, Float) ->
  ViewState ->
  Maybe ViewState
runInstantCommands key keyMods steps zoomCenter viewState =
  case filter isBound instantCommands of
    [] -> Nothing
    command : _ -> Just $ runInstantCommand command steps zoomCenter viewState
  where
    isBound c = isCommand (viewStateCommands viewState) c key keyMods


{-| The commands which take effect immediately,
  in contrast to translating, rotating and scaling by dragging the mouse.
  If an input is bound to several of them, the first one wins.
-}
instantCommands :: [Command]
instantCommands =
  [ CRestore
  , CBumpZoomOut
  , CBumpZoomIn
  , CBumpLeft
  , CBumpRight
  , CBumpUp
  , CBumpDown
  , CBumpClockwise
  , CBumpCClockwise
  ]


-- | Apply an instant command for the given number of steps.
runInstantCommand ::
  Command -> Float -> Maybe (Float, Float) -> ViewState -> ViewState
runInstantCommand command steps zoomCenter viewState =
  case command of
    CRestore -> withPort $ const viewPortInit
    CBumpZoomOut -> withPort $ zoomBy (scaleStep ** steps)
    CBumpZoomIn -> withPort $ zoomBy (1 / scaleStep ** steps)
    CBumpLeft -> withPort $ \port -> motionBump port (20 * steps, 0)
    CBumpRight -> withPort $ \port -> motionBump port (-20 * steps, 0)
    CBumpUp -> withPort $ \port -> motionBump port (0, -20 * steps)
    CBumpDown -> withPort $ \port -> motionBump port (0, 20 * steps)
    CBumpClockwise -> withPort $ rotateBy (5 * steps)
    CBumpCClockwise -> withPort $ rotateBy (-5 * steps)
    CTranslate -> viewState
    CRotate -> viewState
    CScale -> viewState
  where
    scaleStep = viewStateScaleStep viewState
    withPort f = viewState{viewStateViewPort = f (viewStateViewPort viewState)}
    rotateBy degrees port = port{viewPortRotate = viewPortRotate port + degrees}
    zoomBy factor port =
      let port' = port{viewPortScale = viewPortScale port * factor}
      in  case zoomCenter of
            Nothing -> port'
            Just center -> keepPointFixed center port port'


{-| Adjust the translation of the new viewport
  so that the picture point under the given screen point
  is the same as with the old viewport.
-}
keepPointFixed :: (Float, Float) -> ViewPort -> ViewPort -> ViewPort
keepPointFixed screenPoint oldPort newPort =
  newPort
    { viewPortTranslate =
        viewPortTranslate newPort
          Pt.+ invertViewPort newPort screenPoint
          Pt.- invertViewPort oldPort screenPoint
    }


-- | Offset a viewport.
motionBump :: ViewPort -> (Float, Float) -> ViewPort
motionBump
  port@ViewPort
    { viewPortTranslate = trans
    , viewPortScale = scale
    , viewPortRotate = r
    }
  (bumpX, bumpY) =
    port{viewPortTranslate = trans Pt.- o}
    where
      offset = (bumpX / scale, bumpY / scale)
      o = rotateV (degToRad r) offset


-- | Apply a translation to the `ViewState`.
motionTranslate ::
  Maybe (Float, Float) -> -- Location of first mark.
  (Float, Float) -> -- Current position.
  ViewState ->
  Maybe ViewState
motionTranslate Nothing _ _ = Nothing
motionTranslate (Just (markX, markY)) (posX, posY) viewState =
  Just $
    viewState
      { viewStateViewPort = port{viewPortTranslate = trans Pt.- o}
      , viewStateTranslateMark = Just (posX, posY)
      }
  where
    port = viewStateViewPort viewState
    trans = viewPortTranslate port
    scale = viewPortScale port
    r = viewPortRotate port
    dX = markX - posX
    dY = markY - posY
    offset = (dX / scale, dY / scale)
    o = rotateV (degToRad r) offset


-- | Apply a rotation to the `ViewState`.
motionRotate ::
  Maybe (Float, Float) -> -- Location of first mark.
  (Float, Float) -> -- Current position.
  ViewState ->
  Maybe ViewState
motionRotate Nothing _ _ = Nothing
motionRotate (Just (markX, _markY)) (posX, posY) viewState =
  Just $
    viewState
      { viewStateViewPort =
          port{viewPortRotate = rotate - rotateFactor * (posX - markX)}
      , viewStateRotateMark = Just (posX, posY)
      }
  where
    port = viewStateViewPort viewState
    rotate = viewPortRotate port
    rotateFactor = viewStateRotateFactor viewState


-- | Apply a scale to the `ViewState`.
motionScale ::
  Maybe (Float, Float) -> -- Location of first mark.
  (Float, Float) -> -- Current position.
  ViewState ->
  Maybe ViewState
motionScale Nothing _ _ = Nothing
motionScale (Just (_markX, markY)) (posX, posY) viewState =
  Just $
    viewState
      { viewStateViewPort =
          let
            -- Limit the amount of downward scaling so it maxes
            -- out at 1 percent of the original. There's not much
            -- point scaling down to no pixels, or going negative
            -- so that the image is inverted.
            ss =
              if posY > markY
                then scale - scale * (scaleFactor * (posY - markY))
                else scale + scale * (scaleFactor * (markY - posY))

            ss' = max 0.01 ss
          in
            port{viewPortScale = ss'}
      , viewStateScaleMark = Just (posX, posY)
      }
  where
    port = viewStateViewPort viewState
    scale = viewPortScale port
    scaleFactor = viewStateScaleFactor viewState
