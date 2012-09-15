module Main where

import Graphics.Gloss.Interface.Pure.Game

data World = World { picture :: Picture }

winX = 400 :: Int
winY = 300 :: Int

main :: IO ()
main
  = play (InWindow "HGPhyx" (winX*2, winY*2) (10, 10))
    black
    60
    initWorld
    draw
    handleInput
    (\_ s -> s )

initWorld :: World
initWorld = World {
    picture
        = Pictures [Color white $ Pictures gridCenter,
                    Color (makeColor 0.0 0.8 0.5 1.0) $ Pictures $ gridXY 80 5 8]
    }
    where
      gridCenter = [
       Line [(fromIntegral (0-winX), 0), (fromIntegral winX, 0)],
       Line [(0, fromIntegral (0-winY)), (0, fromIntegral winX)]]
      gridXY space x y = xyLines x y
          where
            xyLines x y = xLines x 1 ++ xLines x (0-1) ++ yLines y 1 ++ yLines y (0-1)
            xLines 0 x = []
            xLines n x = Line [(fromIntegral (0-winX), n*space*x), (fromIntegral winX, n*space*x)] : xLines (n-1) x
            yLines 0 y = []
            yLines n y = Line [(n*space*y, fromIntegral (0-winY)), (n*space*y, fromIntegral winX)] : yLines (n-1) y

draw :: World -> Picture
draw (World p) = p

handleInput :: Event -> World -> World
handleInput (EventKey k ks _ _) s = s
handleInput (EventMotion _) s = s


