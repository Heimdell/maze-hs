{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

module Dir where
import Color
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data Dir = N | S | W | E
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance Show Dir where
  show = \case
    N -> color Blue   "▲" -- "⇑"
    E -> color Green  "▶" -- "⇐"
    S -> color Yellow "▼" -- "⇓"
    W -> color Red    "◀" -- "⇒"

un :: Dir -> Dir
un = \case N -> S; S -> N; W -> E; E -> W

next :: Dir -> Dir
next = \case N -> E; E -> S; S -> W; W -> N

mirror :: Dir -> Dir
mirror = \case N -> N; E -> W; S -> S; W -> E

instance Semigroup Dir where
  (<>) N = id
  (<>) E = next
  (<>) S = next . next
  (<>) W = next . next . next