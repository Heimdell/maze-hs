{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import System.IO
import Control.Monad
import Graphics.Gloss.Raster.Field
import Data.Bifunctor
import Data.Array qualified as Array
import Debug.Trace

import Shadow
import Data.Map (Map)
import qualified Local
import qualified Global
import Visibility
import Point
import Control.Lens
import Graphics.Gloss.Interface.IO.Interact
import Dir
import Vec (float)
import Data.Maybe
import System.Exit (exitSuccess)
import qualified Data.HashMap.Strict as HashMap
import IsPoint (step)
import Data.Array (Array)

flTex :: Array (Int, Int) Color
flTex = Array.listArray ((0, 0), (7, 7)) 
  [ o, l, o, o, o, o, l, o 
  , l, l, o, o, o, o, l, l 
  , o, o, l, l, l, l, o, o 
  , o, o, l, o, o, l, o, o 
  , o, o, l, o, o, l, o, o 
  , o, o, l, l, l, l, o, o 
  , l, l, o, o, o, o, l, l 
  , o, l, o, o, o, o, l, o 
  ]
  where
    o, l :: Color
    o = makeColor 0.1 0.1 0.1 1
    l = makeColor 0.2 0.2 0.2 1

fl :: (Int, Int) -> Color
fl = (flTex Array.!)

plTex :: Array (Int, Int) Color
plTex = Array.listArray ((0, 0), (7, 7)) $ reverse
  [ o, o, o, l, o, o, o, o 
  , o, o, l, o, l, o, o, o 
  , o, o, o, l, o, o, o, o 
  , o, l, l, l, l, l, o, o 
  , o, o, l, o, l, o, o, o 
  , o, o, l, o, l, o, o, o 
  , o, o, o, l, o, o, o, o 
  , o, o, o, l, o, o, o, o 
  ]
  where
    o, l :: Color
    o = makeColor 0.1 0.1 0.1  1
    l = makeColor 0.5 0.1 0.15 1

pl :: (Int, Int) -> Color
pl = (plTex Array.!)

wallTex :: Array (Int, Int) Color
wallTex = Array.listArray ((0, 0), (7, 7)) 
  [ l, l, l, l, l, l, l, l 
  , o, o, l, o, o, o, l, o 
  , l, l, l, l, l, l, l, l 
  , o, l, o, o, o, l, o, o 
  , l, l, l, l, l, l, l, l 
  , o, o, l, o, o, o, l, o 
  , l, l, l, l, l, l, l, l 
  , o, l, o, o, o, l, o, o 
  ]
  where
    o, l :: Color
    o = makeColor 0.8 0.2 0.2 1
    l = makeColor 0.4 0.4 0.3 1

wall :: (Int, Int) -> Color 
wall = (wallTex Array.!)

question :: (Int, Int) -> Color
question pt = flip (Array.!) pt $ Array.listArray ((0, 0), (7, 7)) $ reverse
  [ o, o, o, l, l, o, o, o 
  , o, o, l, o, o, l, o, o 
  , o, o, l, o, o, l, o, o 
  , o, o, o, l, o, o, o, o 
  , o, o, o, o, l, o, o, o 
  , o, o, o, o, o, o, o, o 
  , o, o, o, o, l, o, o, o 
  , o, o, o, o, o, o, o, o 
  ]
  where
    o, l :: Color
    o = makeColor 0.2 0.2 0.2 1
    l = makeColor 0.5 0.2 0.5 1

data Game = Game
  { world :: World
  , w, s, a, d :: Bool
  , bg :: HashMap.HashMap Local.Point (Global.Point, Visibility)
  }

game :: Game
game = Game {world, w = False, s = False, a = False, d = False, bg = shadow world}

move :: Game -> Game
move game@Game{w, s, a, d} = game
  & do if w then moveGame N else id
  & do if s then moveGame S else id
  & do if a then moveGame W else id
  & do if d then moveGame E else id

moveGame :: Dir -> Game -> Game
moveGame dir game = game 
  { world = world'
  , bg    = if moved then shad else game.bg
  }
  where
    shad = shadow world'
    (moved, world') = moveWorld dir game.world 

moveWorld :: Dir -> World -> (Bool, World)
moveWorld dir w = do
  let w' = w & start %~ byOneTile (metric w dir)
  let Point {x, y} = w'^.start
  if w^.opaques . at (Global.Global Point {x = round x, y = round y}) . to isJust
  then (False, w)
  else (True, w')

main :: IO ()
main = do
  playFieldIO (InWindow "Maze" (1600, 900) (0, 0)) (4, 4) 24 game
    drawGame
    handleMovement
    simulateTick
  where
    drawGame Game{world, bg} = pure \(fx, fy) -> do
      let x :: Int = round $ fx * (1600 / 8)
      let y :: Int = round $ fy * (900 / 8)
      let (cx, mx) = divMod x 8
      let (cy, my) = divMod y 8
      pickColor world bg cx cy mx my -- fl (my, mx)

    pickColor world bg 0 0 dy dx = pl (dx, dy)
    pickColor world bg x y dy dx = do
      case bg^.at (Local.Local Point {x, y}) of
        Nothing -> fog
        Just (glob, Visibility vis) 
          | world^.opaques . at glob == Just () -> wall (dx, dy)
          | otherwise -> mixColors vis (1 - vis) (fl (dx, dy)) fog
    
      where
        fog = makeColor 0.2 0.2 0.2 1

        dimming vis col = do
          let (r, g, b, a) = rgbaOfColor col
          makeColor (r * vis) (g * vis) (b * vis) a

        -- image :: Map Local.Point (Global.Point, Visibility)
        -- image = world.bg

handleMovement :: Event -> Game -> IO Game
handleMovement e game = case e of
  EventKey a Down c d -> case a of
    Char 'w' -> pure game {w = True}
    Char 's' -> pure game {s = True}
    Char 'a' -> pure game {a = True}
    Char 'd' -> pure game {d = True}
    Char 'g' -> exitSuccess
    _ -> pure game
  EventKey a Up c d -> case a of
    Char 'w' -> pure game {w = False}
    Char 's' -> pure game {s = False}
    Char 'a' -> pure game {a = False}
    Char 'd' -> pure game {d = False}
    Char 'g' -> exitSuccess
    _ -> pure game
  _ -> pure game

simulateTick :: Float -> Game -> IO Game
simulateTick _ = pure . move

byOneTile :: (Global.Point -> Global.Point) -> Point.Point Float -> Point.Point Float
byOneTile f Point {x, y} = unround $ f $ Global.Global Point {x = round x, y = round y}

unround :: Global.Point -> Point.Point Float
unround (Global.Global Point {x, y}) = Point {x = float x, y = float y}
