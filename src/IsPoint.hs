module IsPoint where

import Dir

class IsPoint p where
  origin :: p
  step   :: Dir -> p -> p

steps :: (IsPoint p) => [Dir] -> p -> p
steps = flip (foldr step)
