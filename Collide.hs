module Collide where

import Graphics.Gloss.Data.Picture

import qualified Data.Map as Map
import Particle as P

type SpFlag = ((Int, Int), Bool)
type SpFlags = [SpFlag]

vec2tuple :: Vector -> (Int, Int)
vec2tuple (x, y) = (floor x, floor y)

getDouble :: Particles -> Map.Map (Int, Int) Particle
getDouble s = foldl (flip $ addmap) Map.empty s
    where
        addmap :: Particle -> Map.Map (Int, Int) Particle -> Map.Map (Int, Int) Particle
        addmap p = Map.insert (vec2tuple (x p)) p
    
process :: Particles -> Particles
process = map $ P.moveP . P.gravityP

