module Particle where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

data Particle = Particle {
    x :: Vector,
    colorP :: Color
}

drawP :: Particle -> Picture
drawP (Particle {x = x, colorP = colorP})
    = Color colorP $ Translate (fst x) (snd x) $ ThickCircle 2 4

-- moveP :: Vector -> Particle -> Particle

type Particles = [Particle]

moveP :: Vector -> Particle -> Particle
moveP v p = p {x = (x p) + v}


