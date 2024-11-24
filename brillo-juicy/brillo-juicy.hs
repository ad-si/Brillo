module Main where

import Brillo (
  BitmapData (bitmapSize),
  Display (InWindow),
  Picture (Bitmap),
  display,
  white,
 )
import Brillo.Juicy (loadJuicy)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] ->
      loadJuicy filename
        >>= maybe
          (putStrLn $ "Couldn't load or decode " ++ filename)
          displayPic
    _ ->
      putStrLn
        "Usage: brillo-juicy <file> -- Displays the image in a Brillo window"


displayPic :: Picture -> IO ()
displayPic p@(Bitmap dta) = do
  let (width, height) = bitmapSize dta
  display (InWindow "Image Viewer" (width, height) (10, 10)) white p
displayPic _ = error "only the Bitmap constructor should be used here"
