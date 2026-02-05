module Brillo.Data.Cursor (CursorShape (..))
where


-- | Describes the shape of the mouse cursor.
data CursorShape
  = -- | Standard arrow cursor (default).
    CursorArrow
  | -- | Text input I-beam cursor.
    CursorIBeam
  | -- | Crosshair cursor.
    CursorCrosshair
  | -- | Pointing hand cursor (for clickable elements).
    CursorHand
  | -- | Horizontal resize cursor.
    CursorResizeH
  | -- | Vertical resize cursor.
    CursorResizeV
  | -- | Hidden cursor (invisible).
    CursorHidden
  deriving (Show, Eq, Ord, Bounded, Enum)
