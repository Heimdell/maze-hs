{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}

module Shadow where

import Control.Lens hiding (un)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe, isJust)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)

import Color
import Data.Coerce
import Dir
import Global qualified
import IsPoint
import Local qualified
import Point
import Shadowcasting
import Vec
import Visibility

data World = World
  { _opaques  :: HashSet Global.Point
  , _textures :: HashMap Global.Point Char
  , _portals  :: HashMap Global.Point (HashMap Dir Global.Point)
  , _start    :: Point Float
  }

makeLenses ''World

lunk :: Global.Point -> Dir -> Global.Point -> World -> World
lunk one dir another 
  = link one dir another 
  . link (Global.stepUnsafe (un dir) another) dir (Global.stepUnsafe dir one)


link :: Global.Point -> Dir -> Global.Point -> World -> World
link one dir another 
  = connect one dir another . connect another (un dir) one
  where
    connect :: Global.Point -> Dir -> Global.Point -> World -> World
    connect one' dir' another' =
      portals %~ HashMap.alter (Just . HashMap.insert dir' another' . fold) one'

world :: World
world = World
  { _opaques = walls pic
  , _textures = pic
  , _portals = mempty
  , _start = Point 0 2
  }
    & lunk (Global.Global Point{x = 10, y =  0}) E (Global.Global Point {x = -10, y =  0})
    & lunk (Global.Global Point{x = 10, y =  1}) E (Global.Global Point {x = -10, y =  1})
    & lunk (Global.Global Point{x = 10, y = -1}) E (Global.Global Point {x = -10, y = -1})
    -- & lunk (Global.Global Point{x = 10, y =  2}) E (Global.Global Point {x = -10, y =  2})
    -- & lunk (Global.Global Point{x = 10, y = -2}) E (Global.Global Point {x = -10, y = -2})
    -- & link (Global.Global Point{x = -11, y =  0}) E (Global.Global Point {x = 11, y =  0})
    -- & link (Global.Global Point{x = -11, y =  1}) E (Global.Global Point {x = 11, y =  1})
    -- & link (Global.Global Point{x = -11, y = -1}) E (Global.Global Point {x = 11, y = -1})
    -- & link (Global.Global Point{x = -11, y =  2}) E (Global.Global Point {x = 11, y =  2})
    -- & link (Global.Global Point{x = -11, y = -2}) E (Global.Global Point {x = 11, y = -2})
  where
    -- floors = Map.fromList $ join [[(coerce Point{x, y}, dist x y) | x <- [-10.. 10]] | y <- [-10.. 10]]
    walls = HashMap.keysSet . HashMap.filter (== '▓')
    pic = mempty
       <> xline   10  (-10) (-2) '▓'
       <> xline   10    2    10  '▓'
       <> xline (-10) (-10) (-2) '▓'
       <> xline (-10)   2    10  '▓'
       <> yline   10  (-10) (-2) '▓'
       <> yline   10    2    10  '▓'
       <> yline (-10) (-10) (-2) '▓'
       <> yline (-10)   2    10  '▓'
       
       <> xline   11  (-11) (-2) '▓'
       <> xline   11    2    11  '▓'
       <> xline (-11) (-11) (-2) '▓'
       <> xline (-11)   2    11  '▓'
       <> yline   11  (-11) (-2) '▓'
       <> yline   11    2    11  '▓'
       <> yline (-11) (-11) (-2) '▓'
       <> yline (-11)   2    11  '▓'
       
      --  <> xline (-20) (-20)  20  '▓'
      --  <> xline   20  (-20)  20  '▓'
       
      --  <> yline (-20) (-20)  20  '▓'
      --  <> yline   20  (-20)  20  '▓'
       <> HashMap.singleton (Global.stepUnsafe N Global.origin) 'N'
       <> HashMap.singleton (Global.stepUnsafe E Global.origin) 'E'
       <> HashMap.singleton (Global.stepUnsafe S Global.origin) 'S'
       <> HashMap.singleton (Global.stepUnsafe W Global.origin) 'W'
      --  <> floors

    xline :: Int -> Int -> Int -> Char -> HashMap Global.Point Char
    yline :: Int -> Int -> Int -> Char -> HashMap Global.Point Char
    xline x y0 y1 c = HashMap.fromList [(coerce $ Point x y, c) | y <- [y0.. y1]]
    yline y x0 x1 c = HashMap.fromList [(coerce $ Point x y, c) | x <- [x0.. x1]]

draw :: World -> HashMap Local.Point (Global.Point, Visibility) -> [String]
draw w m = [concat [tex x y | x <- [x0.. x1]] | y <- [y1, y1 - 1.. y0]]
  where
    tex :: Int -> Int -> String
    tex x y = case HashMap.lookup (coerce $ Point x y) m of
      Nothing -> " "
      Just (glob, Visibility v) -> do
        let char = [fromMaybe '░' (HashMap.lookup glob (w ^. textures))]
        v & do
          between     0.0 0.1 (style Faint  $ color Black  char)
            $ between 0.1 0.2 (style Normal $ color Black  char)
            $ between 0.2 0.3 (style Bold   $ color Black  char)
            $ between 0.3 0.4 (style Faint  $ color Green  char)
            $ between 0.4 0.5 (style Normal $ color Green  char)
            $ between 0.5 0.6 (style Bold   $ color Green  char)
            $ between 0.6 0.7 (style Faint  $ color Yellow char)
            $ between 0.7 0.8 (style Normal $ color Yellow char)
            $ between 0.8 0.9 (style Bold   $ color Yellow char)
            $ between 0.9 1.0 (style Normal $ color White  char)
            $ fallback "?"

    x0 = minimum xs
    y0 = minimum ys
    x1 = maximum xs
    y1 = maximum ys
    xs = fmap (.x) points
    ys = fmap (.y) points
    points = HashMap.keys m

between :: Float -> Float -> c -> (Float -> c) -> Float -> c
between low hi res orElse vis
  | low <= vis && vis <= hi = res
  | otherwise               = orElse vis

fallback :: c -> Float -> c
fallback = const

blit :: World -> HashMap Local.Point (Global.Point, Visibility) -> IO ()
blit w m = mapM_ putStrLn $ draw w m

shadow :: World -> HashMap Local.Point (Global.Point, Visibility)
shadow w = Shadowcasting.calculateFOV config origin globalStart 40
  where
    config = Shadowcasting.Config
      { _originOffsetF    = Vec (x - float (round x)) (y - float (round y))
      , _metricFun        = metric w
      , _isTransparentFun = isTransparent
      }

    isTransparent :: Global.Point -> Bool
    isTransparent global = w^.opaques . at global . to isJust . to not

    Point x y = w^.start

    globalStart :: Global.Point
    globalStart = Global.Global Point {x = round x, y = round y}

metric :: World -> Dir -> Global.Point -> Global.Point
metric w dir glob = fromMaybe (Global.stepUnsafe dir glob) do
  fanout <- w^.portals . at glob
  fanout^.at dir
