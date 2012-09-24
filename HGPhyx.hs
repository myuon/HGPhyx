module Main where

import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector as Vector
import qualified Data.Map as Map

import Global
import World
import Particle
import qualified PMap

import Debug.Trace

field :: Vector.Vector Particle
field = Vector.fromList
        [Particle {x = (40, 10), v = (0, 0), f = (0, 0),
                   colorP = makeColor 0.0 0.4 0.8 1.0},
         Particle {x = (10, 200), v = (0, 0), f = (0, 0),
                   colorP = makeColor 0.8 0.4 0.0 1.0}]

main :: IO ()
main
  = play (InWindow "HGPhyx" (winX*2, winY*2) (40, 140))
    black
    60
    (initWorld field)
    draw
    handleInput
    move

draw :: World -> Picture
draw (World {object = obj, grid = grid})
    = Pictures [grid, Pictures $ map drawP (Vector.toList obj)]

handleInput :: Event -> World -> World
handleInput (EventKey k ks _ _) s = s
handleInput (EventMotion _) s = s

move :: Float -> World -> World
move _ w = w {object = process (object w)}
    where
        process :: Particles -> Particles
        process ps = Vector.map (processOne ps) ps
        
        processOne :: Particles -> Particle -> Particle
        processOne ps p = PMap.update ps p . moveP . gravityP $ p



