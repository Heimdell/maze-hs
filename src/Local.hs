{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Local where

import Point qualified
import IsPoint
import GHC.Records
import Data.Hashable (Hashable)

newtype Point = Local { point :: Point.Point Int } 
  deriving newtype (Eq, Ord, IsPoint, Show, Hashable)

instance HasField "x" Point Int where getField loc = loc.point.x
instance HasField "y" Point Int where getField loc = loc.point.y
