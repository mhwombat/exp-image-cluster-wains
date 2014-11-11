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
    Ratchet,
    Checkpoint(..),
    Limit(..),
    getCooperationDeltaE,
    canAdjustCooperationDeltaE,
    adjustCooperationDeltaE,
    getMinAgreementDeltaE,
    canAdjustMinAgreementDeltaE,
    adjustMinAgreementDeltaE,
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
import ALife.Creatur.Persistent (Persistent, mkPersistent, getPS,
  modifyPS, runPS)
import qualified ALife.Creatur.Universe as U
import ALife.Creatur.Util (stateMap)
import ALife.Creatur.Wain.Iomha.ImageDB (ImageDB, mkImageDB)
import ALife.Creatur.Wain.Iomha.Ratchet
  (RatchetSpec, Ratchet, mkRatchet, canAdjust, adjust, currentValue)
import Control.Exception (SomeException, try)
import Control.Monad.State.Lazy (StateT, get)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings)
import Data.Word (Word16)
import System.Directory (makeRelativeToCurrentDirectory)

data Checkpoint = Check Int String Limit deriving (Show, Read)

data Limit = In (Double, Double) | GE Double | LE Double
  deriving (Show, Read)

data Universe a = Universe
  {
    uExperimentName :: String,
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
    uClassifierSizeRange :: (Word16, Word16),
    uDeciderSizeRange :: (Word16, Word16),
    uDevotionRange :: (Double, Double),
    uMaturityRange :: (Word16, Word16),
    uPopulationSize :: Int,
    uPopulationSizeRange :: (Int, Int),
    uReinforcementCount :: Int,
    uEnergyPoolSize :: Double,
    uBaseMetabolismDeltaE :: Double,
    uEnergyCostPerByte :: Double,
    uChildCostFactor :: Double,
    uFlirtingDeltaE :: Double,
    uCooperationDeltaERatchet :: Persistent Ratchet,
    uNoveltyBasedAgreementDeltaE :: Double,
    uMinAgreementDeltaERatchet :: Persistent Ratchet,
    uClassifierR0Range :: (Double,Double),
    uClassifierDRange :: (Double,Double),
    uDeciderR0Range :: (Double,Double),
    uDeciderDRange :: (Double,Double),
    uCheckpoints :: [Checkpoint]
  } deriving Show

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

cClassifierSizeRange :: Setting (Word16, Word16)
cClassifierSizeRange
  = requiredSetting "classifierSizeRange"

cDeciderSizeRange :: Setting (Word16, Word16)
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

cReinforcementCount :: Setting Int
cReinforcementCount = requiredSetting "reinforcementCount"

cEnergyPoolSize :: Setting Double
cEnergyPoolSize = requiredSetting "energyPoolSize"

cBaseMetabolismDeltaE :: Setting Double
cBaseMetabolismDeltaE = requiredSetting "baseMetabDeltaE"

cEnergyCostPerByte :: Setting Double
cEnergyCostPerByte = requiredSetting "energyCostPerByte"

cChildCostFactor :: Setting Double
cChildCostFactor = requiredSetting "childCostFactor"

cFlirtingDeltaE :: Setting Double
cFlirtingDeltaE = requiredSetting "flirtingDeltaE"

cCooperationDeltaERatchet :: Setting RatchetSpec
cCooperationDeltaERatchet = requiredSetting "cooperationDeltaERatchet"

cNoveltyBasedAgreementDeltaE :: Setting Double
cNoveltyBasedAgreementDeltaE
  = requiredSetting "noveltyBasedAgreementDeltaE"

cMinAgreementDeltaERatchet :: Setting RatchetSpec
cMinAgreementDeltaERatchet = requiredSetting "minAgreementDeltaERatchet"

cClassifierR0Range :: Setting (Double,Double)
cClassifierR0Range = requiredSetting "classifierR0Range"

cClassifierDRange :: Setting (Double,Double)
cClassifierDRange = requiredSetting "classifierDecayRange"

cDeciderR0Range :: Setting (Double,Double)
cDeciderR0Range = requiredSetting "deciderR0Range"

cDeciderDRange :: Setting (Double,Double)
cDeciderDRange = requiredSetting "deciderDecayRange"

cCheckpoints :: Setting [Checkpoint]
cCheckpoints = requiredSetting "checkpoints"

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
      uExperimentName = en,
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
      uReinforcementCount = getSetting cReinforcementCount,
      uEnergyPoolSize = getSetting cEnergyPoolSize,
      uBaseMetabolismDeltaE = getSetting cBaseMetabolismDeltaE,
      uEnergyCostPerByte = getSetting cEnergyCostPerByte,
      uChildCostFactor = getSetting cChildCostFactor,
      uFlirtingDeltaE = getSetting cFlirtingDeltaE,
      uCooperationDeltaERatchet
        = mkPersistent
            (mkRatchet . getSetting $ cCooperationDeltaERatchet)
            (workDir ++ "/coop"),
      uNoveltyBasedAgreementDeltaE
        = getSetting cNoveltyBasedAgreementDeltaE,
      uMinAgreementDeltaERatchet
        = mkPersistent
            (mkRatchet . getSetting $ cMinAgreementDeltaERatchet)
            (workDir ++ "/agree"),
      uClassifierR0Range = getSetting cClassifierR0Range,
      uClassifierDRange = getSetting cClassifierDRange,
      uDeciderR0Range = getSetting cDeciderR0Range,
      uDeciderDRange = getSetting cDeciderDRange,
      uCheckpoints = getSetting cCheckpoints
    }
  where en = getSetting cExperimentName
        workDir = getSetting cWorkingDir
        imageDir = getSetting cImageDir

getCooperationDeltaE :: StateT (Universe a) IO Double
getCooperationDeltaE = fmap currentValue $ withCooperationDeltaE getPS

canAdjustCooperationDeltaE :: StateT (Universe a) IO Bool
canAdjustCooperationDeltaE = withCooperationDeltaE . runPS $ canAdjust

adjustCooperationDeltaE
  :: (A.Agent a, D.SizedRecord a)
    => StateT (Universe a) IO ()
adjustCooperationDeltaE = do
  x <- withCooperationDeltaE getPS
  withCooperationDeltaE . modifyPS $ adjust
  x' <- withCooperationDeltaE getPS
  U.writeToLog $
    "Min agreement Δe changed from " ++ show x ++ " to " ++ show x'

withCooperationDeltaE
  :: Monad m => StateT (Persistent Ratchet) m b -> StateT (Universe a) m b
withCooperationDeltaE f = do
  u <- get
  stateMap (\x -> u { uCooperationDeltaERatchet = x })
        uCooperationDeltaERatchet f

getMinAgreementDeltaE :: StateT (Universe a) IO Double
getMinAgreementDeltaE = fmap currentValue $ withCooperationDeltaE getPS

canAdjustMinAgreementDeltaE :: StateT (Universe a) IO Bool
canAdjustMinAgreementDeltaE = withMinAgreementDeltaE . runPS $ canAdjust

adjustMinAgreementDeltaE
  :: (A.Agent a, D.SizedRecord a)
    => StateT (Universe a) IO ()
adjustMinAgreementDeltaE = do
  x <- withMinAgreementDeltaE getPS
  withMinAgreementDeltaE . modifyPS $ adjust
  x' <- withMinAgreementDeltaE getPS
  U.writeToLog $
    "Min agreement Δe changed from " ++ show x ++ " to " ++ show x'

withMinAgreementDeltaE
  :: Monad m => StateT (Persistent Ratchet) m b -> StateT (Universe a) m b
withMinAgreementDeltaE f = do
  u <- get
  stateMap (\x -> u { uMinAgreementDeltaERatchet = x })
        uMinAgreementDeltaERatchet f
