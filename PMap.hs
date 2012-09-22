module PMap where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import qualified Data.Vector as Vector
import qualified Data.Map as Map

import Global
import Particle

type PMap = Map.Map Position Position

toGrid :: Position -> Position
toGrid = mapPair (floor . (/fromIntegral densXY) . fromIntegral)

insert :: Particle -> PMap -> PMap
insert p = Map.insert (toGrid (x p)) (x p)

update :: PMap -> Particle -> Particle -> Particle
update pmap p p' = case isUpdate (x p') (Map.delete (toGrid (x p)) pmap) of
                    Just _  -> reverseV p
                    Nothing -> p'
    where
        isUpdate :: Position -> PMap -> Maybe Position
        isUpdate k = Map.lookup (toGrid k)
        
reverseV :: Particle -> Particle
reverseV p = p{v = mapPair negate (v p)}

