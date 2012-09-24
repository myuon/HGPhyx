module Particle where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import qualified Data.Vector as Vector

import Global


data Particle = Particle {
    x :: Position,
    v :: Vector,
    f :: Vector,
    colorP :: Color
}

type Particles = Vector.Vector Particle

drawP :: Particle -> Picture
drawP (Particle {x = x, colorP = colorP})
    = Color colorP
    $ uncurry Translate (mapPair fromIntegral x)
    $ ThickCircle (fromIntegral radius) (fromIntegral radius*2)


moveP :: Particle -> Particle
moveP p = p {x = mapPair floor
               $ limitXY
               $ v p `plusV` mapPair fromIntegral (x p)
           , v = f p `plusV` v p}

gravityP :: Particle -> Particle
gravityP p = p {f = (0, negate 0.1)}


limitXY :: Point -> Point
limitXY = limitX . limitY
    where
        limitX :: Point -> Point
        limitX v
            | fst v > fromIntegral winX
                = (fromIntegral winX, snd v)
            | fst v < fromIntegral (negate winX)
                = (fromIntegral $ negate winX, snd v)
            | otherwise
                = v
        limitY :: Point -> Point
        limitY v
            | snd v > fromIntegral winY
                = (fst v, fromIntegral winY)
            | snd v < fromIntegral (negate winY)
                = (fst v, fromIntegral $ negate winY)
            | otherwise
                = v

