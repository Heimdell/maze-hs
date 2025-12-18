{-# LANGUAGE DerivingStrategies #-}

module Distance where

newtype Distance = Distance Int 
  deriving newtype (Eq, Ord, Num, Show)
