------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.Universe
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Universe for image mining agents
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Iomha.Universe
  (
    -- * Constructors
    Universe(..),
    loadUniverse,
    U.Agent,
    -- * Lenses
    uExperimentName,
    uClock,
    uLogger,
    uDB,
    uNamer,
    uChecklist,
    uStatsFile,
    uRawStatsFile,
    uFmriDir,
    uShowDeciderModels,
    uShowPredictions,
    uGenFmris,
    uSleepBetweenTasks,
    uImageDB,
    uImageWidth,
    uImageHeight,
    uClassifierSizeRange,
    uDeciderSizeRange,
    uDevotionRange,
    uMaturityRange,
    uInitialPopulationSize,
    uIdealPopulationSize,
    uPopulationAllowedRange,
    uBaseMetabolismDeltaE,
    uEnergyCostPerByte,
    uChildCostFactor,
    uFlirtingDeltaE,
    uCooperationDeltaE,
    uNoveltyBasedAgreementDeltaE,
    uMinAgreementDeltaE,
    uClassifierR0Range,
    uClassifierDRange,
    uDeciderR0Range,
    uDeciderDRange,
    uCheckpoints,
    -- * Other
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
import ALife.Creatur.Persistent (Persistent, mkPersistent)
import qualified ALife.Creatur.Universe as U
import qualified ALife.Creatur.Wain.Checkpoint as CP
import ALife.Creatur.Wain.Iomha.ImageDB (ImageDB, mkImageDB)
import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import Control.Lens hiding (Setting)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings)
import Data.Word (Word16)
import System.Directory (makeRelativeToCurrentDirectory)

data Universe a = Universe
  {
    _uExperimentName :: String,
    _uClock :: K.PersistentCounter,
    _uLogger :: SL.SimpleLogger,
    _uDB :: CFS.CachedFSDatabase a,
    _uNamer :: N.SimpleNamer,
    _uChecklist :: CL.PersistentChecklist,
    _uStatsFile :: FilePath,
    _uRawStatsFile :: FilePath,
    _uFmriDir :: FilePath,
    _uShowDeciderModels :: Bool,
    _uShowPredictions :: Bool,
    _uGenFmris :: Bool,
    _uSleepBetweenTasks :: Int,
    _uImageDB :: ImageDB,
    _uImageWidth :: Int,
    _uImageHeight :: Int,
    _uClassifierSizeRange :: (Word16, Word16),
    _uDeciderSizeRange :: (Word16, Word16),
    _uDevotionRange :: (Double, Double),
    _uMaturityRange :: (Word16, Word16),
    _uInitialPopulationSize :: Int,
    _uIdealPopulationSize :: Int,
    _uPopulationAllowedRange :: (Int, Int),
    _uBaseMetabolismDeltaE :: Double,
    _uEnergyCostPerByte :: Double,
    _uChildCostFactor :: Double,
    _uFlirtingDeltaE :: Double,
    _uCooperationDeltaE :: Persistent Double,
    _uNoveltyBasedAgreementDeltaE :: Double,
    _uMinAgreementDeltaE :: Double,
    _uClassifierR0Range :: (Double,Double),
    _uClassifierDRange :: (Double,Double),
    _uDeciderR0Range :: (Double,Double),
    _uDeciderDRange :: (Double,Double),
    _uCheckpoints :: [CP.Checkpoint]
  } deriving Show
makeLenses ''Universe

instance (A.Agent a, D.SizedRecord a) => U.Universe (Universe a) where
  type Agent (Universe a) = a
  type Clock (Universe a) = K.PersistentCounter
  clock = _uClock
  setClock u c = u { _uClock=c }
  type Logger (Universe a) = SL.SimpleLogger
  logger = _uLogger
  setLogger u l = u { _uLogger=l }
  type AgentDB (Universe a) = CFS.CachedFSDatabase a
  agentDB = _uDB
  setAgentDB u d = u { _uDB=d }
  type Namer (Universe a) = N.SimpleNamer
  agentNamer = _uNamer
  setNamer u n = u { _uNamer=n }
  type Checklist (Universe a) = CL.PersistentChecklist
  checklist = _uChecklist
  setChecklist u cl = u { _uChecklist=cl }

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

cInitialPopulationSize :: Setting Int
cInitialPopulationSize = requiredSetting "initialPopSize"

cIdealPopulationSize :: Setting Int
cIdealPopulationSize = requiredSetting "idealPopSize"

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
cCooperationDeltaE = requiredSetting "initialCooperationDeltaE"

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
      _uExperimentName = en,
      _uClock = K.mkPersistentCounter (workDir ++ "/clock"),
      _uLogger = SL.mkSimpleLogger (workDir ++ "/log/" ++ en ++ ".log"),
      _uDB
        = CFS.mkCachedFSDatabase (workDir ++ "/db")
          (getSetting cCacheSize),
      _uNamer = N.mkSimpleNamer (en ++ "_") (workDir ++ "/namer"),
      _uChecklist = CL.mkPersistentChecklist (workDir ++ "/todo"),
      _uStatsFile = workDir ++ "/statsFile",
      _uRawStatsFile = workDir ++ "/rawStatsFile",
      _uFmriDir = workDir ++ "/log",
      _uShowDeciderModels = getSetting cShowDeciderModels,
      _uShowPredictions = getSetting cShowPredictions,
      _uGenFmris = getSetting cGenFmris,
      _uSleepBetweenTasks = getSetting cSleepBetweenTasks,
      _uImageDB = mkImageDB imageDir,
      _uImageWidth = getSetting cImageWidth,
      _uImageHeight = getSetting cImageHeight,
      _uClassifierSizeRange = getSetting cClassifierSizeRange,
      _uDeciderSizeRange = getSetting cDeciderSizeRange,
      _uDevotionRange = getSetting cDevotionRange,
      _uMaturityRange = getSetting cMaturityRange,
      _uInitialPopulationSize = p0,
      _uIdealPopulationSize = pIdeal,
      _uPopulationAllowedRange = (a', b'),
      _uBaseMetabolismDeltaE = getSetting cBaseMetabolismDeltaE,
      _uEnergyCostPerByte = getSetting cEnergyCostPerByte,
      _uChildCostFactor = getSetting cChildCostFactor,
      _uFlirtingDeltaE = getSetting cFlirtingDeltaE,
      _uCooperationDeltaE
        = mkPersistent initialCooperationDeltaE
            (workDir ++ "/cooperationDeltaE"),
      _uNoveltyBasedAgreementDeltaE
        = getSetting cNoveltyBasedAgreementDeltaE,
      _uMinAgreementDeltaE = getSetting cMinAgreementDeltaE,
      _uClassifierR0Range = getSetting cClassifierR0Range,
      _uClassifierDRange = getSetting cClassifierDRange,
      _uDeciderR0Range = getSetting cDeciderR0Range,
      _uDeciderDRange = getSetting cDeciderDRange,
      _uCheckpoints = getSetting cCheckpoints
    }
  where en = getSetting cExperimentName
        workDir = getSetting cWorkingDir
        imageDir = getSetting cImageDir
        p0 = getSetting cInitialPopulationSize
        pIdeal = getSetting cIdealPopulationSize
        (a, b) = getSetting cPopulationAllowedRange
        a' = round (fromIntegral pIdeal * a)
        b' = round (fromIntegral pIdeal * b)
        initialCooperationDeltaE = getSetting cCooperationDeltaE
