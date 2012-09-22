module PMap where

import Graphics.Gloss.Data.Picture
import qualified Data.Vector as Vector
import qualified Data.Map as Map

import Global
import Particle

type PMap = Map.Map Position Position

insert :: Particle -> PMap -> PMap
insert p = Map.insert (mapPair (floor . (/fromIntegral densXY) . fromIntegral) (x p)) (x p)

update :: PMap -> Particle -> Particle -> Particle
update pmap p p' = case isUpdate (x p') pmap of
                    Just _  -> p'
                    Nothing -> p
    where
        isUpdate :: Position -> PMap -> Maybe Position
        isUpdate k
            = Map.lookup
            (mapPair
                (floor . (/fromIntegral densXY) . fromIntegral) k)


