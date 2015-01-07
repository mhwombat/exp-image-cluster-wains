------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.Action
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ?????
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
module ALife.Creatur.Wain.Iomha.Action
  (
    Action(..)
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Pretty (Pretty)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random, random, randomR)

-- The actions are listed in order of decreasing genetic dominance.
data Action = Cooperate_0 | Cooperate_1 | Cooperate_2 | Cooperate_3
                | Cooperate_4 | Cooperate_5 | Cooperate_6 | Cooperate_7
                | Cooperate_8 | Cooperate_9 | Flirt | Ignore
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance Serialize Action
instance Genetic Action
instance Diploid Action
instance Pretty Action

instance Random Action where
  randomR (a,b) g = (toEnum n, g')
    where (n, g') = randomR (fromEnum a, fromEnum b) g
  random = randomR (minBound,maxBound)
