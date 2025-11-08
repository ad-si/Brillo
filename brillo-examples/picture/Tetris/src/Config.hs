module Config where

-- | Position in blocks in grid
type GridPosition = (Int, Int)
-- | Position in pixels on screen
type Position = (Float, Float)

-- | Window size in pixels
type WindowSize = (Int, Int)
-- Game size in pixels
type GameSize = (Float, Float)
-- Cup size in pixels
type CupSize = (Float, Float)
-- Window position relates to screen
type WindowPosition = (Int, Int)
-- Size of one square block of grid
type BlockSize = Float
-- Size of grid in blocks
type GridSize = (Int, Int)

data AppConfig = AppConfig
    { windowSize :: WindowSize         -- Window size in pixels
    , gameSize :: GameSize             -- Game size in pixels
    , cupSize :: CupSize               -- Cup size in pixels (computed)
    , windowPosition :: WindowPosition -- Window position relates to screen
    , gamePosition :: Position         -- Game position relates to window (computed?)
    , cupPosition :: Position          -- Cup position relates to game (computed?)
    , blockSize :: BlockSize           -- Size of one square block of grid
    , gridSize :: GridSize             -- Size of grid in blocks
    , startPosition :: GridPosition    -- Starting position of the falling figures (computed)
    }

-- | Default app config constructor
defaultAppConfig :: AppConfig
defaultAppConfig = createAppConfig defaultWindowSize defaultGameSize defaultBlockSize
                   defaultGridSize defaultWindowPosition

-- | Creates AppConfig with computed properties
createAppConfig :: WindowSize -> GameSize -> BlockSize -> GridSize -> WindowPosition -> AppConfig
createAppConfig ws gms bs grs wp = AppConfig ws gms (cupSize bs grs) wp (gamePosition gms)
                                   (cupPosition gms) bs grs (startPosition grs)
  where
    cupSize :: BlockSize -> GridSize -> CupSize
    cupSize bs (grw, grh) = (bs * fromIntegral grw, bs * fromIntegral grh)
    gamePosition (gmw, gmh) = (-gmw/2, gmh/2)
    cupPosition gms@(gmw, gmh) = (-97,-200)--let (x,y) = gamePosition gms in
                                --(x + gmw * 0.1, y - gmh * 0.1)
    startPosition (grw, grh) = (div grw 2, grh + 1)

------------------- Default values ------------------

defaultWindowSize :: WindowSize
defaultWindowSize = (810, 600)

defaultGameSize :: GameSize
defaultGameSize = (500, 500)

defaultBlockSize :: BlockSize
defaultBlockSize = 20

defaultGridSize :: GridSize
defaultGridSize = (10, 20)

defaultWindowPosition :: WindowPosition
defaultWindowPosition = (20, 20)
