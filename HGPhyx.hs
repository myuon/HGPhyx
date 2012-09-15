module Main where

import Graphics.Gloss.Interface.Pure.Game

import Global
import World
import Particle

field = [Particle {x = (10, 10), colorP = makeColor 0.0 0.4 0.8 1.0}]

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
    = Pictures $ [Pictures $ (map drawP obj), grid]

handleInput :: Event -> World -> World
handleInput (EventKey k ks _ _) s = s
handleInput (EventMotion _) s = s

move :: Float -> World -> World
move _ w = w {object = map (moveP (0,(0-1))) (object w)}

