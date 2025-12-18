{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}

module Vec where

data Vec a = Vec {x, y :: a}
  deriving stock (Eq, Ord, Functor)

instance Show a => Show (Vec a) where
  show (Vec x y) = "{" <> show x <> ", " <> show y <> "}"

{-# SPECIALISE INLINE rotate45 :: Vec Float  -> Vec Float #-}
{-# SPECIALISE INLINE rotate45 :: Vec Double -> Vec Double #-}
rotate45 :: Floating a => Vec a -> Vec a
rotate45 Vec{x, y} = Vec
  { x = y * sqrt 2 + x * sqrt 2
  , y = x * sqrt 2 - y * sqrt 2
  }

{-# SPECIALISE INLINE float :: Int -> Float #-}
{-# SPECIALISE INLINE float :: Int -> Double #-}
float :: Floating a => Int -> a
float = fromIntegral

{-# SPECIALISE INLINE rot90Vec :: Vec Int -> Vec Int #-}
rot90Vec :: Num a => Vec a -> Vec a
rot90Vec Vec{x, y} = Vec {x = - y, y = x}

instance Num a => Num (Vec a) where
  Vec a b + Vec c d = Vec (a + c) (b + d)
  Vec a b - Vec c d = Vec (a - c) (b - d)
  (*)               = error "Vec.(*)"
  negate pt         = Vec 0 0 - pt
  abs               = error "Vec.abs"
  signum            = error "Vec.signum"
  fromInteger       = error "Vec.fromInteger"
