module Collide where

import Particle

type SpSet = ((Int, Int), Int)
type SpPart = [SpSet]

manage :: Particles -> SpPart
manage p = foldl (regist) [] p

regist :: SpPart -> Particle -> SpPart
regist s Particle {x = x}
    | (cx, cy) `elem` [pos|(pos, cnt)<-s] = inc s (cx, cy)
    | otherwise = add s (cx, cy)
    
    where
        cx = floor $ fst x / 50
        cy = floor $ snd x / 50
        
inc :: SpPart -> (Int, Int) -> SpPart
inc s index = s

add :: SpPart -> (Int, Int) -> SpPart
add s index = s

process :: Particle -> Particle
process = moveP . gravityP

