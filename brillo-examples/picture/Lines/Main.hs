{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo (
  Display (InWindow),
  Picture (Line, LineSmooth, Pictures, ThickLine, ThickLineSmooth, Translate),
  play,
  white,
 )
import Brillo.Interface.Pure.Game (Event)


main :: IO ()
main =
  do
    let state = State []
    play
      (InWindow "Lines" (600, 600) (0, 0))
      white
      100
      state
      makePicture
      handleEvent
      stepWorld


newtype State = State [Picture]


exampleLines :: [Picture]
exampleLines =
  [ Line [(0, 0), (50, 200)]
  , LineSmooth [(20, 0), (70, 200)]
  , ThickLine [(50, 0), (100, 200)] 1
  , ThickLineSmooth [(70, 0), (120, 200)] 1
  , ThickLine [(100, 0), (150, 200)] 2
  , ThickLineSmooth [(120, 0), (170, 200)] 2
  , ThickLine [(150, 0), (200, 200)] 3
  , ThickLineSmooth [(170, 0), (220, 200)] 3
  , ThickLine [(200, 0), (250, 200)] 5
  , ThickLineSmooth [(220, 0), (270, 200)] 5
  , ThickLine [(250, 0), (300, 200)] 8
  , ThickLineSmooth [(270, 0), (320, 200)] 8
  , ThickLine [(300, 0), (350, 200)] 13
  , ThickLineSmooth [(320, 0), (370, 200)] 13
  , ThickLine [(350, 0), (400, 200)] 16
  , ThickLineSmooth [(370, 0), (420, 200)] 16
  , -- Thickness is reset to 1 after each line
    Line [(400, 0), (450, 200)]
    -- -- | This would print an error and look like the previous line,
    -- -- | as the max supported thickness is 16 on macOS.
    -- ThickLine [(450, 0), (500, 200)] 20
    -- , ThickLineSmooth [(470, 0), (520, 200)] 20
  ]


-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture _state =
  Translate (-220) (-80) $ Pictures exampleLines


handleEvent :: Event -> State -> State
handleEvent _event state = state


stepWorld :: Float -> State -> State
stepWorld _ = id
