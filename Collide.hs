module Collide where

import Graphics.Gloss.Data.Picture

import qualified Data.Vector as Vector
import Particle as P

--type SpPart = Map.Map (Int, Int) Int

toGrid :: Vector -> (Int, Int)
toGrid x = (floor $ (fst x)/50, floor $ (snd x)/50)

--makePart :: Particles -> SpPart
--makePart p = Map.fromList (map apply (Map.toList p))
--    where
--        apply :: (Int, Particle) -> ((Int, Int), Int)
--        apply (n, p) = (toGrid (x p), n)
        
--updatePart :: Int -> Particle -> SpPart -> SpPart
--updatePart n p s = s
        
--getCollision :: SpPart -> Int -> Particles -> SpPart
--getCollision s n q
--    | r == Nothing = Map.insert position n s
--    where
--        r = Map.lookup (position) s
--        position = toGrid (x ((Map.!) q n))
        
--collide :: Int -> Int -> Particles -> Particles
--collide m n s = s

process :: Particles -> Particles
process = Vector.map $ P.moveP . P.gravityP

