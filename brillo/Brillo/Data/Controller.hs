
module Brillo.Data.Controller
        ( Controller    (..))
where
import Brillo.Data.ViewPort


-- | Functions to asynchronously control a `Brillo` display.
data Controller
        = Controller
        { -- | Indicate that we want the picture to be redrawn.
          controllerSetRedraw       :: IO ()

          -- | Modify the current viewport, also indicating that it should be redrawn.
        , controllerModifyViewPort  :: (ViewPort -> IO ViewPort) -> IO () }
