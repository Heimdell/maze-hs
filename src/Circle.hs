{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- |
  An interval set for angles.

  Stores intervals of angles in the diapasone [0.. 360].

  Can project tile dimensions into angle interval set. 
  If tile crosses the 360 like [345.. 15], it will be represented 
  as pair [0.. 15] + [345.. 360].

  Angles are internally in radians, btw.
-}
module Circle where

import Control.Lens
import Data.Interval
import Data.IntervalSet qualified as Range

import Vec
import Data.List (intercalate)
import Data.Coerce (coerce)
import Data.Default (Default(def))

{- | 
  Floating-point type we a working with.
-}
type F = Float

{- |
  Angle interval set.
-}
newtype Circle = Circle
  { diapasone :: Range.IntervalSet F
  }
  
instance Default Circle where
  def = full

{- |
  Show will convert angles from radians to grads and then round them.
-}
instance Show Circle where
  show (Circle diapasone)
      = intercalate ", "
      $ show . gradInterval <$> Range.toList diapasone
    where
      gradInterval :: Interval F -> Interval Int
      gradInterval int = (angle'' <$> lowerBound int) <=..<= (angle'' <$> upperBound int)
        where
          angle'' phi = round $ phi * 180 / pi

{- | 
  Full circle [0.. 360].
-}
full :: Circle
full = coerce $ Range.singleton (Finite 0 <=..<= Finite (pi * 2))

{- | 
  Intersection of circles.

  > intersect = coerce Range.intersection
-}
intersect :: Circle -> Circle -> Circle
intersect = coerce Range.intersection

unsee :: Circle -> Circle -> Circle
unsee = coerce Range.difference

{- |
  Sum of remaining subdiapasones.
-}
size :: Circle -> F
size (Circle diapasone) = sum $ width <$> Range.toList diapasone

{- |
  Build a part of a circle representing the tile.
-}
tileInterval :: 
     Vec F    -- ^ origin point /within/ starting tile; @x@ and @y@ are between @[-0.5.. 0.5)@
  -> Vec Int  -- ^ offset from origin to tile, in local coordinates
  -> Circle
tileInterval origin vec = Circle if start > end
    then Range.singleton (Finite start <=..<= Finite (pi * 2))
      <> Range.singleton (Finite 0     <=..<= Finite end)
    else Range.singleton (Finite start <=..<= Finite end)
  where
    (start, end) = bimap angle angle $ diagonal origin vec

{- |
  Project main line segment (that overlaps other in its shadow) onto precise center of view.
  
  The tile position is discrete, so we subtract the offset within origin tile.

  For instance, in the illustration below, internal offset is @Vec 0 (-0.3)@,
  so the bottom edge is going to be closer to @Vec 0 0@ than the top one.

  Effectively, any edge will be translated @- origin@ or @Vec 0 0.3@ in this case.

  >     ┌ ┐
  >        
  >   ┌ ┾━┽ ┐       3
  >    ╱   ╲      4   2
  > ┌ ╁ ┘ └ ╁ ┐
  >   ┃  .  ┃    5  .  1
  > └ ╀ ┐ ┌ ╀ ┘
  >    ╲   ╱      6   8
  >   └ ┾━┽ ┘       7
  >        
  >     └ ┘

  See 'cells'.

-}
diagonal :: 
     Vec F    -- ^ offset within origin tile
  -> Vec Int  -- ^ offset to tile
  -> (Vec F, Vec F)
diagonal start tile@Vec {x, y} = do
  let
    -- build main projection
    (dStart, dEnd) 
      | x >  0 && y >  0 = (Vec   0.5  (-0.5), Vec (-0.5)   0.5 ) -- diag ╲ 2
      | x <  0 && y >  0 = (Vec   0.5    0.5 , Vec (-0.5) (-0.5)) -- diag ╱ 4
      | x <  0 && y <  0 = (Vec (-0.5)   0.5 , Vec   0.5  (-0.5)) -- diag ╲ 6
      | x >  0 && y <  0 = (Vec (-0.5) (-0.5), Vec   0.5    0.5 ) -- diag ╱ 8
      | x == 0 && y >  0 = (Vec   0.5  (-0.5), Vec (-0.5) (-0.5)) -- edge min y 7
      | x == 0 && y <  0 = (Vec (-0.5)   0.5 , Vec   0.5    0.5 ) -- edge max y 3
      | x >  0 && y == 0 = (Vec (-0.5) (-0.5), Vec (-0.5)   0.5 ) -- edge min x 1
      | x <  0 && y == 0 = (Vec   0.5    0.5 , Vec   0.5  (-0.5)) -- edge max x 5
      | x == 0 && y == 0 = (Vec   0      0   , Vec   0      0   ) -- origin tile
  
  -- +-- start or end of the diagonal
  -- |        +-- add offset to the tile
  -- v        v                 v-- move closer to the internal offset
  (  dStart + fmap float tile - start
   , dEnd   + fmap float tile - start
   )

{- |
  Get angle of the vector in diapasone @[0.. pi * 2]@ in radians.
-}
angle :: Vec F -> F
angle Vec {x, y}
  | x >= 0 && y >= 0 = atan (y / x)
  | x <  0 && y >= 0 = pi + atan (y / x)
  | x <  0 && y < 0  = pi + atan (y / x)
  | x >= 0 && y < 0  = 2 * pi + atan (y / x)

{- |
  Get angle of the vector in diapasone @[0.. 360]@ in grad.
-}
angle' :: Vec F -> Int
angle' vec = round $ angle vec * 180 / pi

{- |
  An example arranglement of tiles that gives full shadow.
-}
cells :: [Vec Int]
cells =
  [ Vec 2 0
  , Vec 1 1
  , Vec 0 2
  , Vec (-1) 1
  , Vec (-2) 0
  , Vec (-1) (-1)
  , Vec 0 (-2)
  , Vec 1 (-1)
  ]
