{-# LANGUAGE OverloadedStrings #-}

-- Adapted from ANUPlot version by Clem Baker-Finch
module Main where

import Brillo
import System.Random
import World qualified as W


-- varying prng sequence
main :: IO ()
main =
  do
    gen <- getStdGen
    simulate
      (InWindow "Eden" (800, 600) (10, 10))
      (greyN 0.1) -- background color
      2 -- number of steps per second
      (W.genesis' gen) -- initial world
      W.render -- function to convert world to a Picture
      W.evolve -- function to step the world one iteration
