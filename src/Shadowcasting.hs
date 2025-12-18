{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shadowcasting where

import Control.Lens ((&), use, uses, view, views, (%=), (.=), makeLenses)
import Control.Monad (guard, when, void)
import Control.Monad.Reader (MonadReader, Reader, runReader)
import Control.Monad.State (MonadState, StateT, MonadTrans (lift), execStateT)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Data.Coerce (coerce)
import Data.Default (Default(def))
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import GHC.Generics (Generic)

import Circle
import Dir
import Distance
import Global qualified
import Local qualified
import Point
import Queue (Queue)
import Queue qualified
import Vec
import Visibility

data Shift = DX | DY

data Config = Config
  { _metricFun        :: Dir -> Global.Point -> Global.Point
  , _isTransparentFun :: Global.Point -> Bool
  , _originOffsetF    :: Vec Float
  }

data LocalState = LocalState
  { _queueF   :: Queue (Local.Point, Global.Point, Distance)
  , _resultF  :: HashMap Local.Point (Global.Point, Visibility)
  , _visionF  :: Circle
  , _visitedF :: HashSet Local.Point
  }
  deriving stock (Generic)

instance Default LocalState where
  def = LocalState 
    { _queueF=def
    , _resultF=HashMap.empty
    , _visionF=def
    , _visitedF=HashSet.empty
    }

makeLenses ''Config
makeLenses ''LocalState

type M  = StateT LocalState (Reader Config)
type M' = MaybeT M

type CanCastShadow m = (MonadState LocalState m, MonadReader Config m)

calculateFOV :: Config -> Local.Point -> Global.Point -> Distance -> HashMap Local.Point (Global.Point, Visibility)
calculateFOV env locus global gas
  = emitLightwave locus global gas
  & flip execStateT  def
  & flip runReader env
  & _resultF

emitLightwave :: Local.Point -> Global.Point -> Distance -> M ()
emitLightwave locus global gas = do
    enqueueTile locus global gas
    loop
  where
    loop :: M ()
    loop = do
      dequeued <- uses queueF Queue.tryDequeue
      case dequeued of
        Nothing -> pure ()
        Just (task, remaining) -> do
          queueF .= remaining
          handleNextFOVPoint task
          loop

handleNextFOVPoint :: (Local.Point, Global.Point, Distance) -> M ()
handleNextFOVPoint (locus, global, gas) = do
  skipOnFailure do
    _                    <- visitOnlyOnce locus
    (howVisible, shadow) <- isVisible locus
    _                    <- addPointToFOV locus global howVisible
    transparent          <- isTileTransparent global

    if transparent
    then do
      for_ (neighbors locus.point) \(dir, point') -> do
        global' <- moveGlobalPoint dir global

        lift $ enqueueTile (coerce point') global' (gas - 1)

    else do
      addShadow shadow

skipOnFailure :: M' () -> M ()
skipOnFailure = void . runMaybeT

addShadow :: Circle -> M' ()
addShadow shadow = visionF %= (`unsee` shadow)

enqueueTile :: Local.Point -> Global.Point -> Distance -> M ()
enqueueTile point' global' gas = do
  when (gas > 0) do
    queueF %= Queue.enqueue (point', global', gas)

moveGlobalPoint :: Dir -> Global.Point -> M' Global.Point
moveGlobalPoint dir global = views metricFun \f -> f dir global

addPointToFOV :: Local.Point -> Global.Point -> Visibility -> M' ()
addPointToFOV locus global howVisible = resultF %= HashMap.insert locus (global, howVisible)

isTileTransparent :: Global.Point -> M' Bool
isTileTransparent global = views isTransparentFun ($ global)

visitOnlyOnce :: Local.Point -> M' ()
visitOnlyOnce point = do
  visited <- uses visitedF (HashSet.member point)
  _       <- guard (not visited)

  visitedF %= HashSet.insert point

isVisible :: Local.Point -> M' (Visibility, Circle)
isVisible locus = do
  vision          <- use  visionF
  originOffset    <- view originOffsetF

  let pointCone    = tileInterval originOffset (asVec locus.point)
  let intersection = vision `intersect` pointCone
  let tileSize     = size pointCone
  let visibility   = if tileSize == 0.0 then 1 else size intersection / tileSize

  _               <- guard (visibility > 0)

  pure (Visibility {visibility}, pointCone)
