{-# LANGUAGE DerivingStrategies #-}
module Queue where

import Data.Sequence as Seq
import Data.Default (Default)

newtype Queue a = Queue { chain :: Seq a }
  deriving newtype (Default)

{-# INLINE empty #-}
empty :: Queue a
empty = Queue Seq.empty

{-# INLINE enqueue #-}
enqueue :: a -> Queue a -> Queue a
enqueue item Queue {chain} = Queue (chain :|> item)

{-# INLINE tryDequeue #-}
tryDequeue :: Queue a -> Maybe (a, Queue a)
tryDequeue Queue {chain} = case chain of
  Empty -> Nothing
  item :<| chain' -> Just (item, Queue chain')