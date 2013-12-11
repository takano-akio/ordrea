module FRP.Ordrea
  (
  -- * Basic types
    SignalGen
  , Signal, Event, Discrete

  -- * External events
  , ExternalEvent
  , newExternalEvent, triggerExternalEvent, listenToExternalEvent

  -- * Events
  , generatorE, filterE, stepClockE, dropStepE, eventFromList
  , scanE, mapAccumE, mapAccumEM
  , accumE, scanAccumE, scanAccumEM
  , mapMaybeE, justE, flattenE, expandE, externalE
  , takeWhileE, delayE

  -- * Switchers
  , joinDD, joinDE, joinDS

  -- * Signals
  , start, externalS, joinS, delayS, signalFromList, networkToList
  , networkToListGC

  -- * Discretes
  , accumD, changesD, preservesD, delayD

  -- * Signal-event functions
  , eventToSignal, signalToEvent, applySE

  -- * Signal-discrete functions
  , discreteToSignal

  -- * Overloaded functions
  , TimeFunction(..), (<@>), (<@)

  -- * Errors
  , OrderingViolation (..)
  ) where

import FRP.Ordrea.Base
