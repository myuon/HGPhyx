module Input where

import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector as Vector

import Global
import World
import Particle
import qualified PMap

import Debug.Trace

handleInput :: Event -> World -> World
handleInput (EventKey k ks _ pos) w
  | MouseButton LeftButton <- k, Down <- ks
      = w{ object = addObject pos (object w) }
  | otherwise = w
handleInput (EventMotion a) s = s



