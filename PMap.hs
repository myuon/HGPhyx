module PMap where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Control.Applicative

import Global
import Particle

import Debug.Trace

type PMap = Map.Map Position Position

toGrid :: Position -> Position
toGrid = mapPair (floor . (/fromIntegral densXY) . fromIntegral)

getPMap :: Particles -> PMap
getPMap = Vector.foldl (flip insert) Map.empty
    where
        insert :: Particle -> PMap -> PMap
        insert p = Map.insert (toGrid (x p)) (x p)

update :: Particles -> Particle -> Particle -> Particle
update ps p p' = case isUpdate (x p') pmap' of
                    Just result
                        -> case isTouched result (x p') of
                            True -> collide result p
                            False -> p'{colorP = makeColor 0.8 0.1 0.4 1.0}
                    Nothing -> p'{colorP = orange}
    where
        pmap' :: PMap
        pmap' = Map.delete (toGrid (x p)) $ getPMap ps

        isUpdate :: Position -> PMap -> Maybe Position
        isUpdate k p = checkXY (1, 0)
                   <|> checkXY (1, 1)
                   <|> checkXY (0, 1)
                   <|> checkXY (negate 1, 1)
                   <|> checkXY (negate 1, 0)
                   <|> checkXY (negate 1, negate 1)
                   <|> checkXY (0, negate 1)
                   <|> checkXY (1, negate 1)
                   <|> checkXY (0, 0)
            where
                checkXY :: Position -> Maybe Position
                checkXY = lookXY p k            

lookXY :: PMap -> Position -> Position -> Maybe Position
lookXY pmap k x = Map.lookup (toGrid k `plusP` x) pmap

-- collision
-- (rx, ry)を中心として(vx, vy)方向に速度ベクトルを持つ円が
-- (rx', ry')を中心とする円にぶつかる衝突
-- dx = rx-rx', dy = ry-ry'
--            1       ( dx^2-dy^2    2dxdy     )
-- A = - -----------  (                        )
--       (dx^2+dy^2)  (   2dxdy   -(dx^2-dy^2) )
-- として
-- 反転速度ベクトル(vx', vy')は
-- (vx', vy') = A(vx, vy)
--            = -1/(dx^2+dy^2) ((dx^2-dy^2)vx + 2dxdyvy, 2dxdyvx - (dx^2-dy^2)vy)

collide :: Position -> Particle -> Particle
collide (rx, ry) p = p{v = (vx', vy')}
    where
        dx :: Float
        dx = fromIntegral $ rx - fst (x p)
        dy :: Float
        dy = fromIntegral $ ry - snd (x p)
        
        r2plus = dx^2 + dy^2
        r2minus = dx^2 - dy^2
        r2cross = 2 * dx * dy
        
        vx' = negate (r2minus * fst (v p) + r2cross * snd (v p)) / r2plus
        vy' = negate (r2cross * fst (v p) - r2minus * snd (v p)) / r2plus

isTouched :: Position -> Position -> Bool
isTouched x x' =
    fromIntegral (radius*4)^2 >=
        (uncurry distV2 $ mapPair (mapPair fromIntegral) $ (x, x'))



