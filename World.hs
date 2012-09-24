module World where

import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector as Vector
import qualified Data.Map as Map

import Global
import Particle as P
import qualified PMap

data World = World {
    object :: Particles,
    grid :: Picture
}

initWorld :: Particles -> World
initWorld p = World {
    object
        = p,
    grid
        = Pictures [ Color white
                   $ Pictures gridCenter,
                     Color (makeColor 0.0 0.8 0.5 1.0)
                   $ Pictures
                   $ gridXY (fromIntegral densXY) (floor ((fromIntegral winY)/(fromIntegral densXY))) (floor ((fromIntegral winX)/(fromIntegral densXY)))
                   ]
    }
    where
      gridCenter = [
       Line [(fromIntegral $ negate winX, 0), (fromIntegral winX, 0)],
       Line [(0, fromIntegral $ negate winY), (0, fromIntegral winX)]]
      
      gridXY :: Float -> Int -> Int -> [Picture]
      gridXY space x y = xyLines x y
          where
            xyLines x y = xLines x 1
                       ++ xLines x (negate 1)
                       ++ yLines y 1
                       ++ yLines y (negate 1)

            xLines 0 x = []
            xLines n x = Line [(fromIntegral (negate winX), (fromIntegral n)*space*(fromIntegral x)), 
                               (fromIntegral winX, (fromIntegral n)*space*(fromIntegral x))]
                              : xLines (n-1) x
            yLines 0 y = []
            yLines n y = Line [((fromIntegral n)*space*(fromIntegral y), fromIntegral (negate winY)), 
                               ((fromIntegral n)*space*(fromIntegral y), fromIntegral winX)]
                              : yLines (n-1) y



