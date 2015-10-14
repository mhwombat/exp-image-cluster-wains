------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.GeneratePopulation
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}

import ALife.Creatur (agentId)
import ALife.Creatur.Wain.Iomha.Experiment (ImageWain, randomImageWain,
  printStats)
import ALife.Creatur.Wain (adjustEnergy)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.PersistentStatistics (clearStats)
import ALife.Creatur.Wain.Statistics (Statistic, stats, summarise)
import ALife.Creatur.Wain.Iomha.Universe (Universe(..),
  writeToLog, store, loadUniverse, uClassifierSizeRange,
  uInitialPopulationSize, uStatsFile)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.Random.Class (getRandomR)
import Control.Monad.State.Lazy (StateT, evalStateT, get)

introduceRandomAgent
  :: String -> StateT (Universe ImageWain) IO [Statistic]
introduceRandomAgent name = do
  u <- get
  classifierSize
    <- liftIO . evalRandIO . getRandomR . view uClassifierSizeRange $ u
  agent
    <- liftIO . evalRandIO $
        randomImageWain name u classifierSize
  -- Make the first generation a little hungry so they start learning
  -- immediately.
  -- TODO: Make the amount configurable.
  let (agent', _) = adjustEnergy 0.8 agent
  writeToLog $ "GeneratePopulation: Created " ++ agentId agent'
  writeToLog $ "GeneratePopulation: Stats " ++ pretty (stats agent')
  store agent'
  return (stats agent')

introduceRandomAgents
  :: [String] -> StateT (Universe ImageWain) IO ()
introduceRandomAgents ns = do
  xs <- mapM introduceRandomAgent ns
  let yss = summarise xs
  printStats yss
  statsFile <- use uStatsFile
  clearStats statsFile

main :: IO ()
main = do
  u <- loadUniverse
  let ns = map (("Founder" ++) . show) [1..(view uInitialPopulationSize u)]
  print ns
  evalStateT (introduceRandomAgents ns) u
