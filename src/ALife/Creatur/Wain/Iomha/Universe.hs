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
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module ALife.Creatur.Wain.Iomha.Universe
  (
    Universe(..),
    loadUniverse,
    U.Agent,
    U.agentIds,
    U.currentTime,
    U.genName,
    U.getAgent,
    U.popSize,
    U.store,
    U.writeToLog
  ) where

import qualified ALife.Creatur as A
import qualified ALife.Creatur.Namer as N
import qualified ALife.Creatur.Checklist as CL
import qualified ALife.Creatur.Counter as K
import qualified ALife.Creatur.Database as D
import qualified ALife.Creatur.Database.CachedFileSystem as CFS
import qualified ALife.Creatur.Logger.SimpleLogger as SL
import qualified ALife.Creatur.Universe as U
import qualified ALife.Creatur.Wain.Checkpoint as CP
import ALife.Creatur.Wain.Iomha.ImageDB (ImageDB, mkImageDB)
import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings)
import Data.Word (Word16)
import System.Directory (makeRelativeToCurrentDirectory)

data Universe a = Universe
  {
    uExperimentName :: String,
    uClock :: K.PersistentCounter,
    uLogger :: SL.SimpleLogger,
    uDB :: CFS.CachedFSDatabase a,
    uNamer :: N.SimpleNamer,
    uChecklist :: CL.PersistentChecklist,
    uStatsFile :: FilePath,
    uRawStatsFile :: FilePath,
    uFmriDir :: FilePath,
    uShowDeciderModels :: Bool,
    uShowPredictions :: Bool,
    uGenFmris :: Bool,
    uSleepBetweenTasks :: Int,
    uImageDB :: ImageDB,
    uImageWidth :: Int,
    uImageHeight :: Int,
    uClassifierSizeRange :: (Word16, Word16),
    uDeciderSizeRange :: (Word16, Word16),
    uDevotionRange :: (Double, Double),
    uMaturityRange :: (Word16, Word16),
    uPopulationSize :: Int,
    uPopulationAllowedRange :: (Int, Int),
    uBaseMetabolismDeltaE :: Double,
    uEnergyCostPerByte :: Double,
    uChildCostFactor :: Double,
    uFlirtingDeltaE :: Double,
    uCooperationDeltaE :: Double,
    uNoveltyBasedAgreementDeltaE :: Double,
    uMinAgreementDeltaE :: Double,
    uClassifierR0Range :: (Double,Double),
    uClassifierDRange :: (Double,Double),
    uDeciderR0Range :: (Double,Double),
    uDeciderDRange :: (Double,Double),
    uCheckpoints :: [CP.Checkpoint]
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

requiredSetting :: String -> Setting a
requiredSetting key
  = Setting key (error $ key ++ " not defined in configuration")

cExperimentName :: Setting String
cExperimentName = requiredSetting "experimentName"

cWorkingDir :: Setting FilePath
cWorkingDir = requiredSetting "workingDir"

cCacheSize :: Setting Int
cCacheSize = requiredSetting "cacheSize"

cShowDeciderModels :: Setting Bool
cShowDeciderModels = requiredSetting "showDeciderModels"

cShowPredictions :: Setting Bool
cShowPredictions = requiredSetting "showPredictions"

cGenFmris :: Setting Bool
cGenFmris = requiredSetting "genFMRIs"

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
cPopulationSize = requiredSetting "desiredPopSize"

cPopulationAllowedRange :: Setting (Double, Double)
cPopulationAllowedRange = requiredSetting "popAllowedRange"

cBaseMetabolismDeltaE :: Setting Double
cBaseMetabolismDeltaE = requiredSetting "baseMetabDeltaE"

cEnergyCostPerByte :: Setting Double
cEnergyCostPerByte = requiredSetting "energyCostPerByte"

cChildCostFactor :: Setting Double
cChildCostFactor = requiredSetting "childCostFactor"

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

cClassifierDRange :: Setting (Double,Double)
cClassifierDRange = requiredSetting "classifierDecayRange"

cDeciderR0Range :: Setting (Double,Double)
cDeciderR0Range = requiredSetting "deciderR0Range"

cDeciderDRange :: Setting (Double,Double)
cDeciderDRange = requiredSetting "deciderDecayRange"

cCheckpoints :: Setting [CP.Checkpoint]
cCheckpoints = requiredSetting "checkpoints"

loadUniverse :: IO (Universe a)
loadUniverse = do
  configFile <- Path <$> makeRelativeToCurrentDirectory "iomha.config"
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
      uStatsFile = workDir ++ "/statsFile",
      uRawStatsFile = workDir ++ "/rawStatsFile",
      uFmriDir = workDir ++ "/log",
      uShowDeciderModels = getSetting cShowDeciderModels,
      uShowPredictions = getSetting cShowPredictions,
      uGenFmris = getSetting cGenFmris,
      uSleepBetweenTasks = getSetting cSleepBetweenTasks,
      uImageDB = mkImageDB imageDir,
      uImageWidth = getSetting cImageWidth,
      uImageHeight = getSetting cImageHeight,
      uClassifierSizeRange = getSetting cClassifierSizeRange,
      uDeciderSizeRange = getSetting cDeciderSizeRange,
      uDevotionRange = getSetting cDevotionRange,
      uMaturityRange = getSetting cMaturityRange,
      uPopulationSize = p,
      uPopulationAllowedRange = (a', b'),
      uBaseMetabolismDeltaE = getSetting cBaseMetabolismDeltaE,
      uEnergyCostPerByte = getSetting cEnergyCostPerByte,
      uChildCostFactor = getSetting cChildCostFactor,
      uFlirtingDeltaE = getSetting cFlirtingDeltaE,
      uCooperationDeltaE = getSetting cCooperationDeltaE,
      uNoveltyBasedAgreementDeltaE
        = getSetting cNoveltyBasedAgreementDeltaE,
      uMinAgreementDeltaE = getSetting cMinAgreementDeltaE,
      uClassifierR0Range = getSetting cClassifierR0Range,
      uClassifierDRange = getSetting cClassifierDRange,
      uDeciderR0Range = getSetting cDeciderR0Range,
      uDeciderDRange = getSetting cDeciderDRange,
      uCheckpoints = getSetting cCheckpoints
    }
  where en = getSetting cExperimentName
        workDir = getSetting cWorkingDir
        imageDir = getSetting cImageDir
        p = getSetting cPopulationSize
        (a, b) = getSetting cPopulationAllowedRange
        a' = round (fromIntegral p * a)
        b' = round (fromIntegral p * b)
