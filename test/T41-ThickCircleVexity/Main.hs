module Main where

import Brillo


-- Check that arcs with width 0 are drawn the same way around as arcs with
-- width greater than 0.
main =
  animate (InWindow "Nice Window" (200, 300) (10, 10)) white $
    \time ->
      Pictures
        [ Translate 0 0 $ Rotate (time * 50) $ ThickArc 180 0 80 0
        , Translate 0 (-50) $ Rotate (time * 50) $ ThickArc 180 0 80 5
        , Translate 0 50 $ Rotate (time * 50) $ ThickArc 180 360 80 5
        ]
