module Particle where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import qualified Data.Vector as Vector

import Global

import Debug.Trace

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
    $ circleSolid (fromIntegral radius)


moveP :: Particle -> Particle
moveP p = p {x = mapPair floor
               $ v p `plusV` mapPair fromIntegral (x p)
           , v = mapPair (*0.99) $ f p `plusV` v p
           , f = f p}

gravityP :: Particle -> Particle
gravityP p = p {f = (0, negate constG)}

wallP :: Particle -> Particle
wallP = bounceX . bounceY
    where
        bounceX :: Particle -> Particle
        bounceX p
            | fst (x p) + radius >= winX
                = p {x = (winX - radius, snd (x p)),
                     v = (floor' 0.005 $ negate $ fst (v p), snd (v p)) }
            | fst (x p) - radius <= negate winX
                = p {x = (negate winX + radius, snd (x p)),
                     v = (floor' 0.005 $ negate $ fst (v p), snd (v p)) }
            | otherwise
                = p
        bounceY :: Particle -> Particle
        bounceY p
            | snd (x p) - radius <= negate winY
                = p {x = (fst (x p), negate winY + radius),
                     v = (fst (v p), floor' 0.005 $ negate $ snd (v p)),
                     f = (0, constG) `plusV` (f p) }
            | otherwise
                = p


