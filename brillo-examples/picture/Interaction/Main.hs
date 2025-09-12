{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo
import Brillo.Interface.IO.Interact
import Data.Text qualified as T


handleEvent :: Event -> Int -> IO Int
handleEvent e eventIdx =
  case e of
    EventKey _ Down _ _ -> do
      putStrLn $ unwords ["--- Event", show eventIdx, "---"]
      print e
      return $ eventIdx + 1
    _ -> return eventIdx


-- | Count the events as they are received.
main :: IO ()
main =
  interactIO
    (InWindow "GameEvent" (700, 100) (10, 10))
    white
    0
    (\str -> return $ Translate (-340) 0 $ Scale 0.3 0.3 $ Text $ T.pack $ show str)
    handleEvent
    (const $ return ())
