module Global where

import Graphics.Gloss.Data.Picture
import qualified Control.Arrow as Arrow

winX = 400 :: Int
winY = 300 :: Int

densXY = 50 :: Int

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f = f Arrow.*** f

plusV :: Vector -> Vector -> Vector
plusV (a, a') (b, b') = (a+b, a'+b')

