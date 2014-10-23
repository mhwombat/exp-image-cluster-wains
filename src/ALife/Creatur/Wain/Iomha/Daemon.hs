------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.Daemon
-- Copyright   :  (c) Amy de Buitléir 2013-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- The daemon that runs the Créatúr experiment.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Main where

import ALife.Creatur (programVersion)
import ALife.Creatur.Daemon (CreaturDaemon(..), Job(..),
  simpleDaemon, launch)
import ALife.Creatur.Task (runInteractingAgents, simpleJob)
import ALife.Creatur.Wain (programVersion)
import ALife.Creatur.Wain.Iomha.Wain (ImageWain, run, finishRound)
import ALife.Creatur.Wain.Iomha.Universe (Universe(..),
  writeToLog, replenishEnergyPool, loadUniverse)
import Control.Monad.State (StateT, execStateT, gets)
import Data.Version (showVersion)
import Paths_creatur_wains_iomha (version)
import System.Posix.Daemonize (CreateDaemon(name))

startupHandler :: String -> Universe ImageWain -> IO (Universe ImageWain)
startupHandler programName
  = execStateT (writeToLog $ "Starting " ++ programName)

shutdownHandler :: String -> Universe ImageWain -> IO ()
shutdownHandler programName u =
  execStateT (writeToLog $ "Shutdown requested for " ++ programName) u
  >> return ()

startRoundProgram :: StateT (Universe ImageWain) IO ()
startRoundProgram = do
  p <- gets uEnergyPoolSize
  replenishEnergyPool p

endRoundProgram :: StateT (Universe ImageWain) IO ()
endRoundProgram = do
  f <- gets uStatsFile
  finishRound f

main :: IO ()
main = do
  universe <- loadUniverse
  let program = run universe
  let popRange = uPopulationSizeRange universe
  let message = "creatur-wains-iomha" ++ showVersion version
          ++ ", compiled with " ++ ALife.Creatur.Wain.programVersion
          ++ ", " ++ ALife.Creatur.programVersion
          ++ ", configuration=" ++ show universe
  let j = simpleJob
        { task=runInteractingAgents program popRange
                 startRoundProgram endRoundProgram,
          onStartup=startupHandler message,
          onShutdown=shutdownHandler message,
          sleepTime=uSleepBetweenTasks universe }
  let d = (simpleDaemon j universe)
            { name=Just (uExperimentName universe) }
  let cd = CreaturDaemon d j
  launch cd
