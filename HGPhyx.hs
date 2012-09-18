module Main where

import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as Map

import Global
import World as W
import Particle as P
import Collide as C

field = Map.fromList
        [(0, Particle {x = (10, 10), v = (0, 0), f = (0, 0),
                   colorP = makeColor 0.0 0.4 0.8 1.0}),
         (1, Particle {x = (10, 40), v = (0, 0), f = (0, 0),
                   colorP = makeColor 0.8 0.4 0.0 1.0})]

main :: IO ()
main
  = play (InWindow "HGPhyx" (winX*2, winY*2) (10, 10))
    black
    60
    (initWorld field)
    draw
    handleInput
    move

draw :: World -> Picture
draw (World {object = obj, grid = grid})
    = Pictures [Pictures $ map drawP (Map.elems obj), grid]

handleInput :: Event -> World -> World
handleInput (EventKey k ks _ _) s = s
handleInput (EventMotion _) s = s

move :: Float -> World -> World
move _ w = w {object = C.process (object w)}

