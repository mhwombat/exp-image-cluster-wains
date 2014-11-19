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
import ALife.Creatur.Task (runInteractingAgents, simpleJob, doNothing)
import ALife.Creatur.Wain (programVersion)
import ALife.Creatur.Wain.Iomha.Wain (ImageWain, run, finishRound)
import ALife.Creatur.Wain.Iomha.Universe (Universe(..),
  writeToLog, loadUniverse)
import Control.Concurrent (MVar, newMVar, readMVar, swapMVar)
import Control.Monad (when)
import Control.Monad.State (StateT, execStateT, gets)
import Data.Version (showVersion)
import Paths_creatur_wains_iomha (version)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Daemonize (CreateDaemon(name))

shutdownMessagePrinted :: MVar Bool
shutdownMessagePrinted = unsafePerformIO (newMVar False)

startupHandler :: String -> Universe ImageWain -> IO (Universe ImageWain)
startupHandler programName
  = execStateT (writeToLog $ "Starting " ++ programName)

shutdownHandler :: String -> Universe ImageWain -> IO ()
shutdownHandler programName u = do
  -- Only print the message once
  handled <- readMVar shutdownMessagePrinted
  when (not handled) $ do
    _ <- execStateT (writeToLog $ "Shutdown requested for "
                      ++ programName) u
    _ <- swapMVar shutdownMessagePrinted True
    return ()

endRoundProgram :: StateT (Universe ImageWain) IO ()
endRoundProgram = gets uStatsFile >>= finishRound

main :: IO ()
main = do
  universe <- loadUniverse
  let program = run universe
  let message = "creatur-wains-iomha-" ++ showVersion version
          ++ ", compiled with " ++ ALife.Creatur.Wain.programVersion
          ++ ", " ++ ALife.Creatur.programVersion
          ++ ", configuration=" ++ show universe
  let j = simpleJob
        { task=runInteractingAgents program doNothing endRoundProgram,
          onStartup=startupHandler message,
          onShutdown=shutdownHandler message,
          sleepTime=uSleepBetweenTasks universe }
  let d = (simpleDaemon j universe)
            { name=Just (uExperimentName universe) }
  let cd = CreaturDaemon d j
  launch cd
