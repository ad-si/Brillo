module Brillo.Data.Display (Display (..))
where

import Data.Text (Text)


-- | Describes how Brillo should display its output.
data Display
  = -- | Display in a window with the given name, size and position.
    InWindow Text (Int, Int) (Int, Int)
  | -- | Display full screen.
    FullScreen
  deriving (Eq, Read, Show)
