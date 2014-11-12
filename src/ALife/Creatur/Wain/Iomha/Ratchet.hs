------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.Ratchet
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Iomha.Ratchet
  (
    RatchetSpec(..),
    Ratchet,
    mkRatchet,
    currentValue,
    canAdjust,
    adjust
  ) where

data RatchetSpec
  = RatchetSpec Double Double Double
    deriving (Read, Show)

data Ratchet
  = Ratchet { rStop :: Double,
              rDelta :: Double,
              rCurrent :: Double } deriving (Read, Show)

currentValue :: Ratchet -> Double
currentValue = rCurrent

mkRatchet :: RatchetSpec -> Ratchet
mkRatchet (RatchetSpec a b d) = Ratchet b d a

canAdjust :: Ratchet -> Bool
canAdjust r =
  if rDelta r >= 0
     -- going up
    then rCurrent r < rStop r
    -- going down
    else rCurrent r > rStop r
  

adjust :: Ratchet -> Ratchet
adjust r =
  if rDelta r >= 0
     -- going up
    then r { rCurrent = min (rStop r) (rCurrent r + rDelta r) }
    -- going down
    else r { rCurrent = max (rStop r) (rCurrent r + rDelta r) }
