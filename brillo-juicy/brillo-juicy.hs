module Main where

import Brillo
import Brillo.Juicy
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	case args of
		[filename] -> loadJuicy filename >>= maybe (putStrLn $ "Couldn't load or decode " ++ filename) displayPic
		_          -> putStrLn "Usage: brillo-juicy <file> -- Displays the image in a Brillo window"


displayPic :: Picture -> IO ()
displayPic p@(Bitmap dta) = display (InWindow "Image Viewer" (width, height) (10, 10))
                                                 white
                                                 p
  where (width,height) = bitmapSize dta
displayPic _ = error "only the Bitmap constructor should be used here"
