module Global where

import Graphics.Gloss.Data.Picture
import qualified Control.Arrow as Arrow

type Position = (Int, Int)

winX = 400 :: Int
winY = 300 :: Int

densXY = 80 :: Int

radius = 30 :: Int

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f = f Arrow.*** f

plusV :: Vector -> Vector -> Vector
plusV (a, a') (b, b') = (a+b, a'+b')

plusP :: Position -> Position -> Position
plusP (a, a') (b, b') = (a+b, a'+b')

distV2 :: Vector -> Vector -> Float
distV2 (a, a') (b, b') = (a-b)^2 + (a'-b')^2

