{-# LANGUAGE OverloadedStrings #-}

module Drawing (drawWindow, restartButtonBounds) where

import Config
import Figures
import World

import Control.Monad.Reader
import Control.Monad.State
import Data.Text (pack)

import Brillo


toGlossColor :: GameColor -> Color
toGlossColor Red = makeColorI 224 102 102 255
toGlossColor Yellow = makeColorI 245 220 116 255
toGlossColor Green = makeColorI 180 198 104 255
toGlossColor Blue = makeColorI 71 176 223 255
toGlossColor Violet = makeColorI 190 162 222 255


-- CONSTANT COLORS FOR DRAWING FUNCTIONS

backgroundColor :: Color
backgroundColor = makeColorI 29 31 33 255


textColor :: Color
textColor = makeColorI 197 200 198 255


gridColor :: Color
gridColor = makeColorI 255 255 255 40


cupColor :: Color
cupColor = makeColorI 85 85 85 255


overlayColor :: Color
overlayColor = makeColorI 10 10 10 200


gameOverColor :: Color
gameOverColor = makeColorI 181 7 7 220


restartButtonColor :: Color
restartButtonColor = makeColorI 71 136 183 255


-- | Bounds of the restart button: (x, y, width, height)
restartButtonBounds :: AppConfig -> (Float, Float, Float, Float)
restartButtonBounds conf =
  let (px, py) = cupPosition conf
      (w, h) = cupSize conf
      (cx, cy) = (px + w / 2, py + h / 2)
      bw = 120
      bh = 36
      bx = cx - bw / 2
      by = cy - 85
  in  (bx, by, bw, bh)


-- DRAWING FUNCTIONS

-- | Draws all help components
drawHelp :: StateT TetrisGame (Reader AppConfig) Picture
drawHelp = do
  gameState <- get
  conf <- ask
  let (x, y) = cupPosition conf
  let w = snd $ cupSize conf
  let fp = fontPath conf
  return $
    Pictures $
      writeInfo fp (x - 250, y + w) (-22) (hardness gameState) (score gameState)
  where
    writeInfo fp (x, y) step hard scr =
      snd $
        foldl
          ( \(s, pics) str ->
              ( s + step
              , pics ++ [Color textColor $ Translate x (y + s) $ TrueTypeText fp 14 str]
              )
          )
          (step, [])
          [ "Press P to pause"
          , "or unpause the game."
          , "Press Up Arrow to rotate."
          , "Press Space to drop."
          , ""
          , "Hardness : " <> pack (show hard)
          , "Score " <> pack (show scr)
          ]


-- | Draws a one basic block on the grid
drawBlock :: Block -> Color -> StateT TetrisGame (Reader AppConfig) Picture
drawBlock block blockColor = do
  conf <- ask
  let cp = cupPosition conf
  let sz = blockSize conf
  let bxp = fst cp + ((fromIntegral $ fst block) * sz)
  let byp = snd cp + ((fromIntegral $ snd block) * sz)
  return $
    Color
      blockColor
      ( Polygon
          [ (bxp + 1, byp + 1)
          , (bxp + sz - 1, byp + 1)
          , (bxp + sz - 1, byp + sz - 1)
          , (bxp + 1, byp + sz - 1)
          ]
      )


-- | Draws falling figure of the game state
drawFigure ::
  Color -> GridPosition -> Figure -> StateT TetrisGame (Reader AppConfig) Picture
drawFigure c p f@(Figure _ _ _bs) =
  mapM ((flip drawBlock) c) (getRealCoords f p) >>= return . Pictures


-- | Draws a figure on the grid
drawGrid :: StateT TetrisGame (Reader AppConfig) Picture
drawGrid = do
  gameState <- get
  pics <-
    mapM (\(p, c) -> p `drawBlock` (toGlossColor c)) (getGridAsList gameState)
  return $ Pictures pics


-- | Draws a cup figures are falling into (with empty grid)
drawCup :: StateT TetrisGame (Reader AppConfig) Picture
drawCup = do
  config <- ask
  (x, y) <- fmap cupPosition $ ask
  sz <- fmap cupSize $ ask
  let height = snd sz
  let width = fst sz
  return $
    Pictures
      [ drawEmptyGrid config
      , Color cupColor $
          Line
            [ (x, y + height)
            , (x, y)
            , (x + width, y)
            , (x + width, y + height)
            ]
      ]


-- | Draws empty grid
drawEmptyGrid :: AppConfig -> Picture
drawEmptyGrid conf =
  let cp = cupPosition conf
  in  let gsY = snd $ gridSize conf -- Number of cells along vertical axis
      in  let stepY = (snd $ cupSize conf) / fromIntegral gsY -- step y along vertical axis
          in  let gsX = fst $ gridSize conf -- Number of cells along horizontal axis
              in  let stepX = (fst $ cupSize conf) / fromIntegral gsX -- step x along horizontal axis
                  in  Color gridColor $
                        Pictures [drawHorizontal cp gsY stepY, drawVertical cp gsX stepX]
  where
    drawHorizontal cp gsY stepY =
      let points =
            [ snd cp + stepY * 1
            , snd cp + stepY * 2
            .. snd cp + stepY * (fromIntegral gsY - 1) -- Y line coords along vertical axis
            ]
      in  Pictures $
            zipWith
              (\p1 p2 -> Line [(fst cp, p1), (fst cp + (fst $ cupSize conf), p2)])
              points
              points
    drawVertical cp gsX stepX =
      let points =
            [ fst cp + stepX * 1
            , fst cp + stepX * 2
            .. fst cp + (stepX * fromIntegral gsX - 1) -- X line coords along horizontal axis
            ]
      in  Pictures $
            zipWith
              (\p1 p2 -> Line [(p1, snd cp), (p2, snd cp + (snd $ cupSize conf))])
              points
              points


-- | Draws right sidebar
drawSidebar :: StateT TetrisGame (Reader AppConfig) Picture
drawSidebar = do
  gameState <- get
  conf <- ask
  let fp = fontPath conf
  let fColor = toGlossColor $ case nextColors gameState of
        (c : _) -> c
        [] -> Red -- Default color if list is empty
  pic <-
    drawNextFigure
      fp
      (gridSize conf)
      (getNextFigure gameState)
      fColor
      (blockSize conf)
      (cupPosition conf)
  return $ Pictures [pic]
  where
    drawNextFigure fp pos fig fColor bs cp =
      let np = (fst pos + 3, snd pos - 4)
      in  drawFigure fColor ((\(x, y) -> (x + 1, y - 1)) np) fig
            >>= return
              . Pictures
              . ( :
                    [ Color textColor
                        $ Translate
                          (fst cp + (fromIntegral $ fst np) * bs)
                          (snd cp + (fromIntegral $ (snd np + 3)) * bs)
                        $ TrueTypeText fp 16 "Next"
                    ]
                )


-- | Draws the left game window
drawGame :: StateT TetrisGame (Reader AppConfig) Picture
drawGame = do
  cupPic <- drawCup
  (_x, _y) <- fmap gamePosition $ ask
  return $ Pictures [cupPic]


drawGameOver :: StateT TetrisGame (Reader AppConfig) Picture
drawGameOver = do
  conf <- ask
  gameState <- get
  let (px, py) = cupPosition conf
  let (w, h) = cupSize conf
  let (_winw, _winh) = windowSize conf
  let (winw, winh) = (fromIntegral _winw, fromIntegral _winh)
  let (cx, cy) = (px + w / 2, py + h / 2)
  let fp = fontPath conf
  let overlay =
        Color overlayColor $
          Polygon
            [ (-winw, -winh)
            , (-winw, winh)
            , (winw, winh)
            , (winw, -winh)
            ]
  let (bx, by, bw, bh) = restartButtonBounds conf
  return $
    Pictures
      [ overlay
      , Color gameOverColor $
          Translate (cx - 90) (cy + 20) $
            TrueTypeText fp 36 "Game Over"
      , let scoreStr = show $ score gameState
            scoreWidth = fromIntegral (length scoreStr) * 13
        in  Color textColor $
              Translate (cx - scoreWidth / 2) (cy - 25) $
                TrueTypeText fp 24 $
                  pack scoreStr
      , Color restartButtonColor $
          Polygon
            [ (bx, by)
            , (bx + bw, by)
            , (bx + bw, by + bh)
            , (bx, by + bh)
            ]
      , Color (makeColorI 255 255 255 255) $
          Translate (bx + 30) (by + 10) $
            TrueTypeText fp 18 "Restart"
      ]


drawPauseOverlay :: StateT TetrisGame (Reader AppConfig) Picture
drawPauseOverlay = do
  conf <- ask
  _gameState <- get
  let (px, py) = cupPosition conf
  let (w, h) = cupSize conf
  let (_winw, _winh) = windowSize conf
  let (winw, winh) = (fromIntegral _winw, fromIntegral _winh)
  let (cx, cy) = (px + w / 2, py + h / 2)
  let fp = fontPath conf
  let overlay =
        Color overlayColor $
          Polygon
            [ (-winw, -winh)
            , (-winw, winh)
            , (winw, winh)
            , (winw, -winh)
            ]
  return $
    Pictures
      [ overlay
      , Color textColor $ Translate (cx - 45) cy $ TrueTypeText fp 32 "Pause"
      ]


drawBackground :: StateT TetrisGame (Reader AppConfig) Picture
drawBackground = do
  conf <- ask
  let (winw, winh) = windowSize conf
  let (x, y) = ((fromIntegral winw) / 2, (fromIntegral winh) / 2)
  let overlay =
        Color backgroundColor $
          Polygon
            [ (-x, -y)
            , (-x, y)
            , (x, y)
            , (x, -y)
            ]
  return overlay


-- | Draws the whole window picture
drawWindow :: StateT TetrisGame (Reader AppConfig) Picture
drawWindow = do
  backgroundPic <- drawBackground
  gamePic <- drawGame
  gameState <- get
  let pos = fallingPosition gameState
  let fig = fallingFigure gameState
  let fColor = toGlossColor $ fallingColor gameState
  figurePic <- drawFigure fColor pos fig
  sidebarPic <- drawSidebar
  helpPic <- drawHelp
  gridPic <- drawGrid
  gameOverOverlayPic <- drawGO $ gameOver gameState
  pauseOverlayPic <- drawPO (isPause gameState) (gameOver gameState)
  linkPic <- drawL (isPause gameState)
  return $
    Pictures
      [ backgroundPic
      , gridPic
      , gamePic
      , figurePic
      , sidebarPic
      , helpPic
      , gameOverOverlayPic
      , pauseOverlayPic
      , linkPic
      ]
  where
    drawGO True = drawGameOver
    drawGO False = return $ Pictures []
    drawPO True False = drawPauseOverlay
    drawPO _ _ = return $ Pictures []
    drawL False = return $ Pictures []
    drawL True = return $ Pictures [] -- Add True case to complete pattern
