module Main where

import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector as Vector
import qualified Data.Map as Map

import Global
import World
import Particle
import Input
import qualified PMap

import Debug.Trace

main :: IO ()
main
  = play (InWindow "HGPhyx" (winX*2, winY*2) (40, 140))
    black
    60
    initWorld
    draw
    handleInput
    move

draw :: World -> Picture
draw (World {object = obj, grid = grid})
    = Pictures [grid, Pictures $ map drawP (Vector.toList obj), color (makeColor 1.0 1.0 1.0 1.0) $ Text (show $ Vector.length $ obj)]

move :: Float -> World -> World
move _ w = w {object = process (object w)}
    where
        process :: Particles -> Particles
        process ps = Vector.map ((processOne ps) . (\p -> p {f = (0, 0)})) ps
        
        processOne :: Particles -> Particle -> Particle
        processOne ps p =
--            trace ("x:" ++ show (x p) ++ "\tv:" ++ show (v p) ++ "\ta:" ++ show (f p)) $
            PMap.update ps p . moveP . wallP . gravityP $ p



