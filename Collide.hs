module Collide where

import Particle as P

type SpSet = ((Int, Int), Int)
type SpPart = [SpSet]

manage :: Particles -> SpPart
manage = foldl regist []

regist :: SpPart -> Particle -> SpPart
regist s Particle {x = x}
    | (cx, cy) `elem` [pos|(pos, cnt)<-s] = inc s (cx, cy)
    | otherwise = add s (cx, cy)
    where
        cx = floor $ fst x / 50
        cy = floor $ snd x / 50
        
inc :: SpPart -> (Int, Int) -> SpPart
inc s index = map (isEqual index) s
    where
        isEqual index (pos, cnt)
            | index == pos = (pos, cnt+1)
            | otherwise    = (pos, cnt)

add :: SpPart -> (Int, Int) -> SpPart
add s (cx, cy) = small ++ [((cx, cy), 0)] ++ large
    where
        small = [((sx, sy), cnt) |((sx, sy), cnt)<-s, sx * 100 + sy < cx * 100 + cy]
        large = [((sx, sy), cnt) |((sx, sy), cnt)<-s, sx * 100 + sy > cx * 100 + cy]



process :: Particles -> Particles
process = map $ P.moveP . P.gravityP

