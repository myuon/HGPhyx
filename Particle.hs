module Particle where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

import Global

type Particles = [Particle]

data Particle = Particle {
    x :: Vector,
    v :: Vector,
    f :: Vector,
    colorP :: Color
}

drawP :: Particle -> Picture
drawP (Particle {x = x, colorP = colorP})
    = Color colorP $ uncurry Translate x $ ThickCircle 3 6


moveP :: Particle -> Particle
moveP p = p {x = limitXY $ v p + x p, v = f p + v p}

gravityP :: Particle -> Particle
gravityP p = p {f = (0, negate 0.1)}


limitXY :: Vector -> Vector
limitXY = limitX . limitY
    where
        limitX :: Vector -> Vector
        limitX v
            | fst v > fromIntegral winX
                = (fromIntegral winX, snd v)
            | fst v < fromIntegral (negate winX)
                = (fromIntegral $ negate winX, snd v)
            | otherwise
                = v
        limitY :: Vector -> Vector
        limitY v
            | snd v > fromIntegral winY
                = (fst v, fromIntegral winY)
            | snd v < fromIntegral (negate winY)
                = (fst v, fromIntegral $ negate winY)
            | otherwise
                = v

