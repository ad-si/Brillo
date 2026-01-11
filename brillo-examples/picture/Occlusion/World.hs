{-# LANGUAGE ScopedTypeVariables #-}

module World where

import Brillo.Algorithms.RayCast
import Brillo.Data.Extent
import Brillo.Data.QuadTree
import Brillo.Interface.Pure.Game
import Cell


-- | The game world.
data World
  = World
  { worldWidth :: Int
  , worldHeight :: Int
  , worldTree :: QuadTree Cell
  , worldCellSize :: Int
  , worldCellSpace :: Int
  }
  deriving (Show)


-- | Get the extent covering the entire world.
worldExtent :: World -> Extent
worldExtent world =
  makeExtent (worldWidth world) 0 (worldHeight world) 0


-- | Load a world from a file.
loadWorld :: FilePath -> IO World
loadWorld fileName =
  do
    str <- readFile fileName
    return $ readWorld str


-- | Read a world from a string.
readWorld :: String -> World
readWorld str =
  case lines str of
    ("WORLD" : strWidthHeight : _ : cellLines) ->
      case map read $ words strWidthHeight of
        [width, height] ->
          let rows = take height cellLines
              cells =
                concat $
                  map (readLine width) $
                    reverse rows
              extent = makeExtent height 0 width 0
          in  World
                { worldWidth = width
                , worldHeight = height
                , worldTree = makeWorldTree extent cells
                , worldCellSize = 20
                , worldCellSpace = 0
                }
        _ -> error "readWorld: invalid width/height format"
    _ -> error "readWorld: invalid world format"


readLine :: Int -> String -> [Cell]
readLine width (_ : str) =
  map readCell $
    take width str
readLine _ [] = []


-- | Get the size of the window needed to display a world.
windowSizeOfWorld :: World -> (Int, Int)
windowSizeOfWorld world =
  let cellSize = worldCellSize world
      cellSpace = worldCellSpace world
      cellPad = cellSize + cellSpace
      height = cellPad * (worldHeight world) + cellSpace
      width = cellPad * (worldWidth world) + cellSpace
  in  (width, height)


-- | Create the tree representing the world from a list of all its cells.
makeWorldTree :: Extent -> [Cell] -> QuadTree Cell
makeWorldTree extent cells =
  foldr insert' emptyTree nonEmptyPosCells
  where
    insert' (pos, cell) tree =
      case insertByCoord extent pos cell tree of
        Nothing -> tree
        Just tree' -> tree'

    (width, height) =
      sizeOfExtent extent

    posCells =
      zip
        [ (x, y)
        | y <- [0 .. height - 1]
        , x <- [0 .. width - 1]
        ]
        cells

    nonEmptyPosCells =
      filter (\x -> snd x /= CellEmpty) posCells


-- | Get the world position coresponding to a point in the window.
worldPosOfWindowPos :: World -> Point -> Point
worldPosOfWindowPos world (x, y) =
  let (windowSizeX, windowSizeY) =
        windowSizeOfWorld world

      offsetX = fromIntegral $ windowSizeX `div` 2
      offsetY = fromIntegral $ windowSizeY `div` 2

      scaleFactor = fromIntegral $ worldCellSize world

      x' = (x + offsetX) / scaleFactor
      y' = (y + offsetY) / scaleFactor
  in  (x', y')


-- | Check if a the cell at a given coordinate is visible from a point.
cellAtCoordIsVisibleFromCoord :: World -> Coord -> Coord -> Bool
cellAtCoordIsVisibleFromCoord world cFrom cTo =
  let (cx, cy) = cFrom
      pFrom = (fromIntegral cx + 0.5, fromIntegral cy + 0.5)
  in  cellAtCoordIsVisibleFromPoint world pFrom cTo


{-| Check if a cell at a given coordinate is visible from a point.
     We say it's visible if the center of any of its faces is visible.
-}
cellAtCoordIsVisibleFromPoint :: World -> Point -> Coord -> Bool
cellAtCoordIsVisibleFromPoint world pFrom (x', y') =
  or $ map (cellAtPointIsVisibleFromPoint world pFrom) [pa, pb, pc, pd]
  where
    x :: Float = fromIntegral x' + 0.5
    y :: Float = fromIntegral y' + 0.5
    pa = (x - 0.4999, y)
    pb = (x + 0.4999, y)
    pc = (x, y - 0.4999)
    pd = (x, y + 0.4999)


-- | Check if a point on some cell (P2) is visible from some other point (P1).
cellAtPointIsVisibleFromPoint :: World -> Point -> Point -> Bool
cellAtPointIsVisibleFromPoint world p1 p2 =
  let mOccluder = castSegIntoWorld world p1 p2
  in  case mOccluder of
        Nothing -> False
        Just (_, extent, _) -> pointInExtent extent p2


-- | Given a line segment (P1-P2) get the cell closest to P1 that intersects the segment.
castSegIntoWorld :: World -> Point -> Point -> Maybe (Point, Extent, Cell)
castSegIntoWorld world p1 p2 =
  castSegIntoCellularQuadTree p1 p2 (worldExtent world) (worldTree world)


-- | Given a line segment (P1-P2) get the cell closest to P1 that intersects the segment.
traceSegIntoWorld :: World -> Point -> Point -> [(Point, Extent, Cell)]
traceSegIntoWorld world p1 p2 =
  traceSegIntoCellularQuadTree p1 p2 (worldExtent world) (worldTree world)
