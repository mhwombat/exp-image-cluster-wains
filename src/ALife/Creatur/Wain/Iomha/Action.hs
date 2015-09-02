------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.Action
-- Copyright   :  (c) Amy de Buitléir 2012-2015
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
    Action(..),
    numActions
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Pretty (Pretty)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random, random, randomR)

-- The actions are listed in order of decreasing genetic dominance.
data Action = Cooperate_a | Cooperate_b | Cooperate_c | Cooperate_d
                | Cooperate_e | Cooperate_f | Cooperate_g | Cooperate_h
                | Cooperate_i | Cooperate_j | Cooperate_k | Cooperate_l
                | Cooperate_m | Cooperate_n | Cooperate_o | Cooperate_p
                | Flirt | Ignore
                -- | Cooperate_q | Cooperate_r | Cooperate_s | Cooperate_t
                -- | Cooperate_u | Cooperate_v | Cooperate_w | Cooperate_x
                -- | Cooperate_y | Cooperate_z | Flirt | Ignore
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance Serialize Action
instance Genetic Action
instance Diploid Action
instance Pretty Action

instance Random Action where
  randomR (a,b) g = (toEnum n, g')
    where (n, g') = randomR (fromEnum a, fromEnum b) g
  random = randomR (minBound,maxBound)

numActions :: Int
numActions = 1 + fromEnum (maxBound :: Action)
