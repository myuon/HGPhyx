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
    = Pictures [grid, Pictures $ map drawP (Vector.toList obj)]

move :: Float -> World -> World
move _ w = w {object = process (object w)}
    where
        process :: Particles -> Particles
        process ps = Vector.map (processOne ps) ps
        
        processOne :: Particles -> Particle -> Particle
        processOne ps p = PMap.update ps p . moveP . gravityP $ p



