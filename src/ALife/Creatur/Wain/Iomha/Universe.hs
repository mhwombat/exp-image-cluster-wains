------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.Universe
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Universe for image mining agents
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module ALife.Creatur.Wain.Iomha.Universe
  (
    Universe(..),
    loadUniverse,
    U.Agent,
    U.agentIds,
    U.currentTime,
    U.getAgent,
    U.popSize,
    U.replenishEnergyPool,
    U.store,
    U.withdrawEnergy,
    U.writeToLog
  ) where

import qualified ALife.Creatur as A
import qualified ALife.Creatur.Namer as N
import qualified ALife.Creatur.Checklist as CL
import qualified ALife.Creatur.Counter as K
import qualified ALife.Creatur.Database as D
import qualified ALife.Creatur.Database.CachedFileSystem as CFS
import qualified ALife.Creatur.Logger.SimpleLogger as SL
import qualified ALife.Creatur.EnergyPool as E
import ALife.Creatur.Wain.Iomha.ImageDB (ImageDB, mkImageDB)
import qualified ALife.Creatur.Universe as U
import Control.Exception (SomeException, try)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings)
import Data.Word (Word8, Word16)
import System.Directory (makeRelativeToCurrentDirectory)

data Universe a = Universe
  {
    uClock :: K.PersistentCounter,
    uLogger :: SL.SimpleLogger,
    uDB :: CFS.CachedFSDatabase a,
    uNamer :: N.SimpleNamer,
    uChecklist :: CL.PersistentChecklist,
    uEnergyPool :: E.PersistentEnergyPool,
    uStatsFile :: FilePath,
    uRawStatsFile :: FilePath,
    uSleepBetweenTasks :: Int,
    uImageDB :: ImageDB,
    uImageWidth :: Int,
    uImageHeight :: Int,
    uClassifierSizeRange :: (Word8, Word8),
    uDeciderSizeRange :: (Word8, Word8),
    uDevotionRange :: (Double, Double),
    uMaturityRange :: (Word16, Word16),
    uPopulationSize :: Int,
    uPopulationSizeRange :: (Int, Int),
    uEnergyPoolSize :: Double,
    uBaseMetabolismDeltaE :: Double,
    uEnergyCostPerByte :: Double,
    uChildCostFactor :: Double,
    uEasementTime :: Int,
    uEasementCooperationDeltaE :: Double,
    uEasementAgreementDeltaE :: Double,
    uFlirtingDeltaE :: Double,
    uCooperationDeltaE :: Double,
    uNoveltyBasedAgreementDeltaE :: Double,
    uMinAgreementDeltaE :: Double,
    uClassifierR0Range :: (Double,Double),
    uClassifierRfRange :: (Double,Double),
    uClassifierW0Range :: (Double,Double),
    uClassifierWfRange :: (Double,Double),
    uClassifierTfRange :: (Double,Double),
    uDeciderR0Range :: (Double,Double),
    uDeciderRfRange :: (Double,Double),
    uDeciderW0Range :: (Double,Double),
    uDeciderWfRange :: (Double,Double),
    uDeciderTfRange :: (Double,Double),
    uMinAvgEnergy :: Double,
    uMinAvgClassifierIQ :: Double,
    uMinAvgDeciderIQ :: Double,
    uMinAvgFlirted :: Double,
    uMinAvgCooperation :: Double,
    uMinAvgNetDeltaE :: Double,
    uPhase1MinAvgAge :: Double,
    uPhase1MinAvgSQ :: Double,
    uPhase1MinAvgAgreed :: Double
  } deriving (Show, Eq)

instance (A.Agent a, D.SizedRecord a) => U.Universe (Universe a) where
  type Agent (Universe a) = a
  type Clock (Universe a) = K.PersistentCounter
  clock = uClock
  setClock u c = u { uClock=c }
  type Logger (Universe a) = SL.SimpleLogger
  logger = uLogger
  setLogger u l = u { uLogger=l }
  type AgentDB (Universe a) = CFS.CachedFSDatabase a
  agentDB = uDB
  setAgentDB u d = u { uDB=d }
  type Namer (Universe a) = N.SimpleNamer
  agentNamer = uNamer
  setNamer u n = u { uNamer=n }
  type Checklist (Universe a) = CL.PersistentChecklist
  checklist = uChecklist
  setChecklist u cl = u { uChecklist=cl }
  type EnergyPool (Universe a) = E.PersistentEnergyPool
  energyPool = uEnergyPool
  setEnergyPool u cl = u { uEnergyPool=cl }

requiredSetting :: String -> Setting a
requiredSetting key
  = Setting key (error $ key ++ " not defined in configuration")

cExperimentName :: Setting String
cExperimentName = requiredSetting "experimentName"

cWorkingDir :: Setting FilePath
cWorkingDir = requiredSetting "workingDir"

cCacheSize :: Setting Int
cCacheSize = requiredSetting "cacheSize"

cSleepBetweenTasks :: Setting Int
cSleepBetweenTasks = requiredSetting "sleepTimeBetweenTasks"

cImageDir :: Setting FilePath
cImageDir = requiredSetting "imageDir"

cImageWidth :: Setting Int
cImageWidth = requiredSetting "imageWidth"

cImageHeight :: Setting Int
cImageHeight = requiredSetting "imageHeight"

cClassifierSizeRange :: Setting (Word8, Word8)
cClassifierSizeRange
  = requiredSetting "classifierSizeRange"

cDeciderSizeRange :: Setting (Word8, Word8)
cDeciderSizeRange
  = requiredSetting "deciderSizeRange"
    
cDevotionRange :: Setting (Double, Double)
cDevotionRange
  = requiredSetting "devotionRange"

cMaturityRange :: Setting (Word16, Word16)
cMaturityRange = requiredSetting "maturityRange"

cPopulationSize :: Setting Int
cPopulationSize = requiredSetting "initialPopSize"

cPopulationSizeRange :: Setting (Int, Int)
cPopulationSizeRange = requiredSetting "popSizeRange"

cEnergyPoolSize :: Setting Double
cEnergyPoolSize = requiredSetting "energyPoolSize"

cBaseMetabolismDeltaE :: Setting Double
cBaseMetabolismDeltaE = requiredSetting "baseMetabDeltaE"

cEnergyCostPerByte :: Setting Double
cEnergyCostPerByte = requiredSetting "energyCostPerByte"

cChildCostFactor :: Setting Double
cChildCostFactor = requiredSetting "childCostFactor"

cEasementTime :: Setting Int
cEasementTime = requiredSetting "easementTime"

cEasementCooperationDeltaE :: Setting Double
cEasementCooperationDeltaE = requiredSetting "easementCooperationDeltaE"

cEasementAgreementDeltaE :: Setting Double
cEasementAgreementDeltaE = requiredSetting "easementAgreementDeltaE"

cFlirtingDeltaE :: Setting Double
cFlirtingDeltaE = requiredSetting "flirtingDeltaE"

cCooperationDeltaE :: Setting Double
cCooperationDeltaE = requiredSetting "cooperationDeltaE"

cNoveltyBasedAgreementDeltaE :: Setting Double
cNoveltyBasedAgreementDeltaE
  = requiredSetting "noveltyBasedAgreementDeltaE"

cMinAgreementDeltaE :: Setting Double
cMinAgreementDeltaE = requiredSetting "minAgreementDeltaE"

cClassifierR0Range :: Setting (Double,Double)
cClassifierR0Range = requiredSetting "classifierR0Range"

cClassifierRfRange :: Setting (Double,Double)
cClassifierRfRange = requiredSetting "classifierRfRange"

cClassifierW0Range :: Setting (Double,Double)
cClassifierW0Range = requiredSetting "classifierW0Range"

cClassifierWfRange :: Setting (Double,Double)
cClassifierWfRange = requiredSetting "classifierWfRange"

cClassifierTfRange :: Setting (Double,Double)
cClassifierTfRange = requiredSetting "classifierTfRange"

cDeciderR0Range :: Setting (Double,Double)
cDeciderR0Range = requiredSetting "deciderR0Range"

cDeciderRfRange :: Setting (Double,Double)
cDeciderRfRange = requiredSetting "deciderRfRange"

cDeciderW0Range :: Setting (Double,Double)
cDeciderW0Range = requiredSetting "deciderW0Range"

cDeciderWfRange :: Setting (Double,Double)
cDeciderWfRange = requiredSetting "deciderWfRange"

cDeciderTfRange :: Setting (Double,Double)
cDeciderTfRange = requiredSetting "deciderTfRange"

cMinAvgEnergy :: Setting Double
cMinAvgEnergy = requiredSetting "stopIfAvgEnergyLessThan"

cMinAvgClassifierIQ :: Setting Double
cMinAvgClassifierIQ = requiredSetting "stopIfAvgClassifierIQLessThan"

cMinAvgDeciderIQ :: Setting Double
cMinAvgDeciderIQ = requiredSetting "stopIfAvgDeciderIQLessThan"

cMinAvgCooperation :: Setting Double
cMinAvgCooperation = requiredSetting "stopIfAvgCooperationLessThan"

cMinAvgNetDeltaE :: Setting Double
cMinAvgNetDeltaE = requiredSetting "stopIfAvgNetDeltaELessThan"

cPhase1MinAvgAge :: Setting Double
cPhase1MinAvgAge = requiredSetting "afterEasementStopIfAvgAgeLessThan"

cPhase1MinAvgSQ :: Setting Double
cPhase1MinAvgSQ = requiredSetting "afterEasementStopIfAvgSQLessThan"

cPhase1MinAvgAgreed :: Setting Double
cPhase1MinAvgAgreed
  = requiredSetting "afterEasementStopIfAvgAgreedLessThan"

cMinAvgFlirted :: Setting Double
cMinAvgFlirted = requiredSetting "afterEasementStopIfAgvFlirtedLessThan"

loadUniverse :: IO (Universe a)
loadUniverse = do
  configFile <- fmap Path $ makeRelativeToCurrentDirectory "iomha.config"
  readResult <- try $ readSettings configFile
  case readResult of
 	  Right (_, GetSetting getSetting) -> return $
            config2Universe getSetting
 	  Left (x :: SomeException) -> error $
            "Error reading the config file: " ++ show x

config2Universe :: (forall a. Read a => Setting a -> a) -> Universe b
config2Universe getSetting =
  Universe
    {
      uClock = K.mkPersistentCounter (workDir ++ "/clock"),
      uLogger = SL.mkSimpleLogger (workDir ++ "/log/" ++ en ++ ".log"),
      uDB
        = CFS.mkCachedFSDatabase (workDir ++ "/db")
          (getSetting cCacheSize),
      uNamer = N.mkSimpleNamer (en ++ "_") (workDir ++ "/namer"),
      uChecklist = CL.mkPersistentChecklist (workDir ++ "/todo"),
      uEnergyPool = E.mkPersistentEnergyPool (workDir ++ "/energy"),
      uStatsFile = workDir ++ "/statsFile",
      uRawStatsFile = workDir ++ "/rawStatsFile",
      uSleepBetweenTasks = getSetting cSleepBetweenTasks,
      uImageDB = mkImageDB imageDir,
      uImageWidth = getSetting cImageWidth,
      uImageHeight = getSetting cImageHeight,
      uClassifierSizeRange = getSetting cClassifierSizeRange,
      uDeciderSizeRange = getSetting cDeciderSizeRange,
      uDevotionRange = getSetting cDevotionRange,
      uMaturityRange = getSetting cMaturityRange,
      uPopulationSize = getSetting cPopulationSize,
      uPopulationSizeRange = getSetting cPopulationSizeRange,
      uEnergyPoolSize = getSetting cEnergyPoolSize,
      uBaseMetabolismDeltaE = getSetting cBaseMetabolismDeltaE,
      uEnergyCostPerByte = getSetting cEnergyCostPerByte,
      uChildCostFactor = getSetting cChildCostFactor,
      uEasementTime = getSetting cEasementTime,
      uEasementCooperationDeltaE
        = getSetting cEasementCooperationDeltaE,
      uEasementAgreementDeltaE = getSetting cEasementAgreementDeltaE,
      uFlirtingDeltaE = getSetting cFlirtingDeltaE,
      uCooperationDeltaE = getSetting cCooperationDeltaE,
      uNoveltyBasedAgreementDeltaE
        = getSetting cNoveltyBasedAgreementDeltaE,
      uMinAgreementDeltaE = getSetting cMinAgreementDeltaE,
      uClassifierR0Range = getSetting cClassifierR0Range,
      uClassifierRfRange = getSetting cClassifierRfRange,
      uClassifierW0Range = getSetting cClassifierW0Range,
      uClassifierWfRange = getSetting cClassifierWfRange,
      uClassifierTfRange = getSetting cClassifierTfRange,
      uDeciderR0Range = getSetting cDeciderR0Range,
      uDeciderRfRange = getSetting cDeciderRfRange,
      uDeciderW0Range = getSetting cDeciderW0Range,
      uDeciderWfRange = getSetting cDeciderWfRange,
      uDeciderTfRange = getSetting cDeciderTfRange,
      uMinAvgEnergy = getSetting cMinAvgEnergy,
      uMinAvgClassifierIQ = getSetting cMinAvgClassifierIQ,
      uMinAvgDeciderIQ = getSetting cMinAvgDeciderIQ,
      uMinAvgFlirted = getSetting cMinAvgFlirted,
      uMinAvgCooperation = getSetting cMinAvgCooperation,
      uMinAvgNetDeltaE = getSetting cMinAvgNetDeltaE,
      uPhase1MinAvgAge = getSetting cPhase1MinAvgAge,
      uPhase1MinAvgSQ = getSetting cPhase1MinAvgSQ,
      uPhase1MinAvgAgreed = getSetting cPhase1MinAvgAgreed
    }
  where en = getSetting cExperimentName
        workDir = getSetting cWorkingDir
        imageDir = getSetting cImageDir
