{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass #-}

module Point where

import Dir
import Vec
import Color
import IsPoint
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Point a = Point {x, y :: a}
  deriving stock (Eq, Ord, Functor, Generic)
  deriving anyclass (Hashable)

instance (Num a, Eq a, Show a) => Show (Point a) where
  show Point {x, y} = color Cyan "x" <> show' x <> color Magenta " y" <> show' y
    where
      show' i = case signum i of
        -1 -> color Red           (show i)
        1  -> color Green  ("+" ++ show i)
        0  -> color Yellow (" " ++ show i)

{-# SPECIALISE INLINE step :: Dir -> Point Int -> Point Int #-}
instance Num a => IsPoint (Point a) where
  step N pt@Point{y} = pt { y = y + 1 }
  step S pt@Point{y} = pt { y = y - 1 }
  step E pt@Point{x} = pt { x = x + 1 }
  step W pt@Point{x} = pt { x = x - 1 }

  origin = Point {x = 0, y = 0}

{- |

                  .
                  ↑
               .←-.-→.
               ↑  ↑  ↑
            .←-.←-.-→.-→.
            ↑  ↑  ↑  ↑  ↑
         .←-.←-.←-x-→.-→.-→.
            ↓  ↓  ↓  ↓  ↓
            .←-.←-.-→.-→.
               ↓  ↓  ↓
               .←-.-→.
                  ↓
                  .
-}
{-# SPECIALISE INLINE neighbors :: Point Int -> [(Dir, Point Int)] #-}
neighbors :: forall a. (Eq a, Num a) => Point a -> [(Dir, Point a)]
neighbors pt@Point {x, y} = case (signum x, signum y) of
  ( 0,  0) -> [inDir N, inDir E, inDir S, inDir W]
  ( 1,  0) -> [inDir S, inDir E, inDir N]  -- >
  (-1,  0) -> [inDir N, inDir W, inDir S]  -- <
  ( 0,  1) -> [inDir E, inDir N, inDir W]  -- ^
  ( 0, -1) -> [inDir W, inDir S, inDir E]  -- v
  ( 1,  1) -> [inDir E, inDir N]           -- \->
  (-1,  1) -> [inDir N, inDir W]           -- <-/
  (-1, -1) -> [inDir W, inDir S]           -- <-\
  ( 1, -1) -> [inDir S, inDir E]           -- /->

  where
    inDir :: Dir -> (Dir, Point a)
    inDir dir = (dir, step dir pt)

asVec :: Point a -> Vec a
asVec (Point a b) = Vec a b