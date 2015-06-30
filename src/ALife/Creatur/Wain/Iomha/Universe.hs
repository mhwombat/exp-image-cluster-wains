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
    uFmriCounter,
    uFmriDir,
    uShowDeciderModels,
    uShowPredictions,
    uGenFmris,
    uSleepBetweenTasks,
    uImageDB,
    uImageWidth,
    uImageHeight,
    uInitialImageRange,
    uClassifierSizeRange,
    uDeciderSizeRange,
    uDevotionRange,
    uMaturityRange,
    uMaxAge,
    uInitialPopulationSize,
    uIdealPopulationSize,
    uPopulationAllowedRange,
    uFrequencies,
    uBaseMetabolismDeltaE,
    uEnergyCostPerByte,
    uChildCostFactor,
    uFlirtingDeltaE,
    uCSQDeltaE,
    uDSQDeltaE,
    uDQDeltaE,
    uCooperationDeltaE,
    uPopControlDeltaE,
    uDQBasedAgreementDeltaE,
    uNoveltyBasedAgreementDeltaE,
    uMinAgreementDeltaE,
    uAdultAdultTeaching,
    uOutcomeRange,
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
import ALife.Creatur.Wain.PlusMinusOne (PM1Double)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Exception (SomeException, try)
import Control.Lens hiding (Setting)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings)
import Data.Word (Word8, Word16)
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
    _uFmriCounter :: K.PersistentCounter,
    _uFmriDir :: FilePath,
    _uShowDeciderModels :: Bool,
    _uShowPredictions :: Bool,
    _uGenFmris :: Bool,
    _uSleepBetweenTasks :: Int,
    _uImageDB :: ImageDB,
    _uImageWidth :: Int,
    _uImageHeight :: Int,
    _uInitialImageRange :: (Word8, Word8),
    _uClassifierSizeRange :: (Word16, Word16),
    _uDeciderSizeRange :: (Word16, Word16),
    _uDevotionRange :: (UIDouble, UIDouble),
    _uMaturityRange :: (Word16, Word16),
    _uMaxAge :: Int,
    _uInitialPopulationSize :: Int,
    _uIdealPopulationSize :: Int,
    _uPopulationAllowedRange :: (Int, Int),
    _uFrequencies :: [Rational],
    _uBaseMetabolismDeltaE :: Double,
    _uEnergyCostPerByte :: Double,
    _uChildCostFactor :: Double,
    _uFlirtingDeltaE :: Double,
    _uCSQDeltaE :: Double,
    _uDSQDeltaE :: Double,
    _uDQDeltaE :: Double,
    _uCooperationDeltaE :: Double,
    _uPopControlDeltaE :: Persistent Double,
    _uAdultAdultTeaching :: Bool,
    _uDQBasedAgreementDeltaE :: Double,
    _uNoveltyBasedAgreementDeltaE :: Double,
    _uMinAgreementDeltaE :: Double,
    _uOutcomeRange :: (PM1Double, PM1Double),
    _uClassifierR0Range :: (UIDouble, UIDouble),
    _uClassifierDRange :: (UIDouble, UIDouble),
    _uDeciderR0Range :: (UIDouble, UIDouble),
    _uDeciderDRange :: (UIDouble, UIDouble),
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

cInitialImageRange :: Setting (Word8, Word8)
cInitialImageRange = requiredSetting "initialImageRange"

cClassifierSizeRange :: Setting (Word16, Word16)
cClassifierSizeRange
  = requiredSetting "classifierSizeRange"

cDeciderSizeRange :: Setting (Word16, Word16)
cDeciderSizeRange
  = requiredSetting "deciderSizeRange"
    
cDevotionRange :: Setting (UIDouble, UIDouble)
cDevotionRange
  = requiredSetting "devotionRange"

cMaturityRange :: Setting (Word16, Word16)
cMaturityRange = requiredSetting "maturityRange"

cMaxAge :: Setting Int
cMaxAge = requiredSetting "maxAge"

cInitialPopulationSize :: Setting Int
cInitialPopulationSize = requiredSetting "initialPopSize"

cIdealPopulationSize :: Setting Double
cIdealPopulationSize = requiredSetting "idealPopSize"

cPopulationAllowedRange :: Setting (Double, Double)
cPopulationAllowedRange = requiredSetting "popAllowedRange"

cFrequencies :: Setting [Rational]
cFrequencies = requiredSetting "frequencies"

cBaseMetabolismDeltaE :: Setting Double
cBaseMetabolismDeltaE = requiredSetting "baseMetabDeltaE"

cEnergyCostPerByte :: Setting Double
cEnergyCostPerByte = requiredSetting "energyCostPerByte"

cChildCostFactor :: Setting Double
cChildCostFactor = requiredSetting "childCostFactor"

cFlirtingDeltaE :: Setting Double
cFlirtingDeltaE = requiredSetting "flirtingDeltaE"

cCSQDeltaE :: Setting Double
cCSQDeltaE = requiredSetting "csqDeltaE"

cDSQDeltaE :: Setting Double
cDSQDeltaE = requiredSetting "dsqDeltaE"

cDQDeltaE :: Setting Double
cDQDeltaE = requiredSetting "dqDeltaE"

cCooperationDeltaE :: Setting Double
cCooperationDeltaE = requiredSetting "cooperationDeltaE"

cAdultAdultTeaching :: Setting Bool
cAdultAdultTeaching = requiredSetting "adultAdultTeaching"

cDQBasedAgreementDeltaE :: Setting Double
cDQBasedAgreementDeltaE = requiredSetting "dqBasedAgreementDeltaE"

cNoveltyBasedAgreementDeltaE :: Setting Double
cNoveltyBasedAgreementDeltaE
  = requiredSetting "noveltyBasedAgreementDeltaE"

cMinAgreementDeltaE :: Setting Double
cMinAgreementDeltaE = requiredSetting "minAgreementDeltaE"

cOutcomeRange :: Setting (PM1Double, PM1Double)
cOutcomeRange = requiredSetting "outcomeRange"

cClassifierR0Range :: Setting (UIDouble, UIDouble)
cClassifierR0Range = requiredSetting "classifierR0Range"

cClassifierDRange :: Setting (UIDouble, UIDouble)
cClassifierDRange = requiredSetting "classifierDecayRange"

cDeciderR0Range :: Setting (UIDouble, UIDouble)
cDeciderR0Range = requiredSetting "deciderR0Range"

cDeciderDRange :: Setting (UIDouble, UIDouble)
cDeciderDRange = requiredSetting "deciderDecayRange"

cCheckpoints :: Setting [CP.Checkpoint]
cCheckpoints = requiredSetting "checkpoints"

loadUniverse :: IO (Universe a)
loadUniverse = do
  configFile <- Path <$> makeRelativeToCurrentDirectory "iomha.config"
  readResult <- try $ readSettings configFile
  case readResult of
    Right (_, GetSetting getSetting) ->
      return $ config2Universe getSetting
    Left (x :: SomeException) ->
      error $ "Error reading the config file: " ++ show x

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
      _uFmriCounter = K.mkPersistentCounter (workDir ++ "/fmriCount"),
      _uFmriDir = workDir ++ "/log",
      _uShowDeciderModels = getSetting cShowDeciderModels,
      _uShowPredictions = getSetting cShowPredictions,
      _uGenFmris = getSetting cGenFmris,
      _uSleepBetweenTasks = getSetting cSleepBetweenTasks,
      _uImageDB = mkImageDB imageDir,
      _uImageWidth = getSetting cImageWidth,
      _uImageHeight = getSetting cImageHeight,
      _uInitialImageRange = getSetting cInitialImageRange,
      _uClassifierSizeRange = getSetting cClassifierSizeRange,
      _uDeciderSizeRange = getSetting cDeciderSizeRange,
      _uDevotionRange = getSetting cDevotionRange,
      _uMaturityRange = getSetting cMaturityRange,
      _uMaxAge = getSetting cMaxAge,
      _uInitialPopulationSize = p0,
      _uIdealPopulationSize = pIdeal,
      _uPopulationAllowedRange = (a', b'),
      _uFrequencies = getSetting cFrequencies,
      _uBaseMetabolismDeltaE = getSetting cBaseMetabolismDeltaE,
      _uEnergyCostPerByte = getSetting cEnergyCostPerByte,
      _uChildCostFactor = getSetting cChildCostFactor,
      _uFlirtingDeltaE = getSetting cFlirtingDeltaE,
      _uCSQDeltaE = getSetting cCSQDeltaE,
      _uDSQDeltaE = getSetting cDSQDeltaE,
      _uDQDeltaE = getSetting cDQDeltaE,
      _uCooperationDeltaE = getSetting cCooperationDeltaE,
      _uPopControlDeltaE
        = mkPersistent 0 (workDir ++ "/popControlDeltaE"),
      _uAdultAdultTeaching = getSetting cAdultAdultTeaching,
      _uDQBasedAgreementDeltaE = getSetting cDQBasedAgreementDeltaE,
      _uNoveltyBasedAgreementDeltaE
        = getSetting cNoveltyBasedAgreementDeltaE,
      _uMinAgreementDeltaE = getSetting cMinAgreementDeltaE,
      _uOutcomeRange = getSetting cOutcomeRange,
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
        fIdeal = getSetting cIdealPopulationSize
        pIdeal = round (fromIntegral p0 * fIdeal)
        (a, b) = getSetting cPopulationAllowedRange
        a' = round (fromIntegral pIdeal * a)
        b' = round (fromIntegral pIdeal * b)
