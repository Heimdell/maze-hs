module Visibility where

newtype Visibility = Visibility {visibility :: Float}

instance Show Visibility where
  show Visibility {visibility} = take 3 (show visibility)