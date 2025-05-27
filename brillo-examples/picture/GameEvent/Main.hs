{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo
import Data.Text qualified as T


-- | Display the last event received as text.
main :: IO ()
main =
  play
    (InWindow "GameEvent" (700, 100) (10, 10))
    white
    100
    ""
    (\str -> Translate (-340) 0 $ Scale 0.1 0.1 $ Text str)
    (\event _ -> T.pack $ show event)
    (\_ world -> world)
