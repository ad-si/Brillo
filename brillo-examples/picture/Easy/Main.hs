
module Main where

import Brillo

main :: IO ()
main = display (InWindow "My Window" (200, 200) (10, 10)) white (Circle 80)

