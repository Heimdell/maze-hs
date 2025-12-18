{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Global where

import Point qualified
import Dir (Dir)
import Data.Coerce
import qualified IsPoint
import GHC.Records
import Data.Hashable (Hashable)

newtype Point = Global { point :: Point.Point Int } 
  deriving newtype (Eq, Ord, Show, Hashable)

stepUnsafe :: Dir -> Point -> Point
stepUnsafe = coerce (IsPoint.step @(Point.Point Int))

stepsUnsafe :: [Dir] -> Point -> Point
stepsUnsafe = flip (foldr stepUnsafe)

origin :: Point
origin = coerce (IsPoint.origin @(Point.Point Int))

instance HasField "x" Point Int where getField glob = glob.point.x
instance HasField "y" Point Int where getField glob = glob.point.y
