module Main where

import Graphics.Gloss.Interface.Pure.Game

data World = World {
    object :: Picture,
    grid :: Picture }

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
    (\_ s -> s{ object = translate 1 0 (object s) } )

initWorld :: World
initWorld = World {
    object
        = Pictures [Color (makeColor 1.0 0.0 0.0 1.0) $ Translate 100 0 $ ThickCircle 10 20,
                    Color (makeColor 0.0 0.0 1.0 1.0) $ Translate 0 100 $ ThickCircle 10 20],
    grid
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
            xLines n x = Line [(fromIntegral (0-winX), n*space*x), 
                               (fromIntegral winX, n*space*x)] : xLines (n-1) x
            yLines 0 y = []
            yLines n y = Line [(n*space*y, fromIntegral (0-winY)), 
                               (n*space*y, fromIntegral winX)] : yLines (n-1) y

draw :: World -> Picture
draw (World p g) = Pictures [p, g]

handleInput :: Event -> World -> World
handleInput (EventKey k ks _ _) s
    | SpecialKey KeyDown <- k, Down <- ks
        = s{ object = translate 0 20 (object s)}
    | otherwise = s
handleInput (EventMotion _) s = s


