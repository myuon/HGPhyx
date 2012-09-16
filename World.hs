module World where

import Graphics.Gloss.Interface.Pure.Game

import Global
import Particle as P

data World = World {
    object :: Particles,
    grid :: Picture
}

initWorld :: Particles -> World
initWorld p = World {
    object
        = p,
    grid
        = Pictures [Color white $ Pictures gridCenter,
                    Color (makeColor 0.0 0.8 0.5 1.0) $ Pictures $ gridXY 80 5 8]
    }
    where
      gridCenter = [
       Line [(fromIntegral $ negate winX, 0), (fromIntegral winX, 0)],
       Line [(0, fromIntegral $ negate winY), (0, fromIntegral winX)]]
      gridXY space x y = xyLines x y
          where
            xyLines x y = xLines x 1 ++ xLines x (negate 1) ++ yLines y 1 ++ yLines y  (negate 1)
            xLines 0 x = []
            xLines n x = Line [(fromIntegral (negate winX), n*space*x), 
                               (fromIntegral winX, n*space*x)] : xLines (n-1) x
            yLines 0 y = []
            yLines n y = Line [(n*space*y, fromIntegral (negate winY)), 
                               (n*space*y, fromIntegral winX)] : yLines (n-1) y



