-- This program used to leak about 25k/sec.
-- Space leak was in the timing code.
module Main where

import Brillo
import Brillo.Interface.IO.Game


main =
  playIO
    (InWindow "BrilloMem" (500, 500) (0, 0))
    white
    10
    0
    (\world -> return (translate (-250) 0 (text $ show world)))
    (\event -> (\world -> return world))
    (\timePassed -> (\world -> return $ world + timePassed))
