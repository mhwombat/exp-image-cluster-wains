------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.Wain
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables,
    TemplateHaskell, Rank2Types #-}
module ALife.Creatur.Wain.Iomha.Wain
  (
    ImageWain,
    run,
    randomImageWain,
    finishRound,
    energy,
    passion,
    schemaQuality,
    printStats
  ) where

import Prelude hiding (lookup)
import ALife.Creatur (agentId)
import ALife.Creatur.Database (size)
import ALife.Creatur.Util (stateMap)
import ALife.Creatur.Task (requestShutdown)
import ALife.Creatur.Wain (Wain(..), Label, adjustEnergy, adjustPassion,
  chooseAction, buildWainAndGenerateGenome, classify, teachLabel,
  incAge, weanMatureChildren, tryMating, energy,
  passion, hasLitter, reflect)
import ALife.Creatur.Wain.Brain (classifier, buildBrain)
import qualified ALife.Creatur.Wain.ClassificationMetrics as SQ
import ALife.Creatur.Wain.GeneticSOM (RandomDecayingGaussianParams(..),
  randomDecayingGaussian, buildGeneticSOM, numModels, counterMap)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, randomResponse, action)
import ALife.Creatur.Wain.Util (unitInterval)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Creatur.Wain.Iomha.Action (Action(..))
import ALife.Creatur.Wain.Iomha.Image (Image, stripedImage, randomImage)
import ALife.Creatur.Wain.Iomha.ImageDB (ImageDB, anyImage)
import qualified ALife.Creatur.Wain.Iomha.Universe as U
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats)
import ALife.Creatur.Wain.Statistics (lookup, summarise)
import Control.Lens hiding (Action, universe)
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandomR)
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT, get,
  gets)
import Data.List (intercalate)
import Data.Word (Word8)
import Math.Geometry.GridMap (elems)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import System.Random (randomIO, randomRIO)
import Text.Printf (printf)

data Object = IObject Image String | AObject ImageWain

isImage :: Object -> Bool
isImage (IObject _ _) = True
isImage (AObject _) = False

objectId :: Object -> String
objectId (IObject _ s) = "Image " ++ s
objectId (AObject a) = agentId a

objectAppearance :: Object -> Image
objectAppearance (IObject img _) = img
objectAppearance (AObject a) = appearance a

addIfAgent :: Object -> [ImageWain] -> [ImageWain]
addIfAgent (IObject _ _) xs = xs
addIfAgent (AObject a) xs = a:xs

randomlyInsertImages :: ImageDB -> [Object] -> IO [Object]
randomlyInsertImages db xs = do
  insert <- randomIO
  if insert
    then do
      (img, imageId) <- evalStateT anyImage db
      n <- randomRIO (0, 1)
      let (fore, aft) = splitAt n xs
      randomlyInsertImages db $ fore ++ IObject img imageId : aft
    else
      return xs

type ImageWain = Wain Image Action

-- TODO: Redo with lenses

randomImageWain
  :: RandomGen r
    => String -> U.Universe ImageWain -> Word8 -> Word8 -> Rand r ImageWain
randomImageWain wainName u classifierSize deciderSize = do
  let n = fromIntegral $ 3*classifierSize*classifierSize
  let w = U.uImageWidth u
  let h = U.uImageHeight u
  imgs <- replicateM n (randomImage w h)
  let fcp = RandomDecayingGaussianParams
               { r0Range = U.uClassifierR0Range u,
                 rfRange = U.uClassifierRfRange u,
                 w0Range = U.uClassifierW0Range u,
                 wfRange = U.uClassifierWfRange u,
                 tfRange = U.uClassifierTfRange u,
                 sideLength = classifierSize }
  fc <- randomDecayingGaussian fcp
  let c = buildGeneticSOM classifierSize fc imgs
  let fdp = RandomDecayingGaussianParams
              { r0Range = U.uDeciderR0Range u,
                rfRange = U.uDeciderRfRange u,
                w0Range = U.uDeciderW0Range u,
                wfRange = U.uDeciderWfRange u,
                tfRange = U.uDeciderTfRange u,
                sideLength = deciderSize }
  fd <- randomDecayingGaussian fdp
  xs <- replicateM
         (numTiles . snd . U.uDeciderSizeRange $ u)
         $ randomResponse (numModels c) 
  let b = buildBrain c (buildGeneticSOM deciderSize fd xs)
  d <- getRandomR (U.uDevotionRange u)
  m <- getRandomR (U.uMaturityRange u)
  p <- getRandomR unitInterval
  let app = stripedImage w h
  return $ buildWainAndGenerateGenome wainName app b d m p

numTiles :: Word8 -> Int
numTiles s = 3*s'*(s'-1) + 1
  where s' = fromIntegral s

data Summary = Summary
  {
    _rPopSize :: Int,
    _rDirectObjectNovelty :: Double,
    _rDirectObjectAdjustedNovelty :: Int,
    _rIndirectObjectNovelty :: Double,
    _rIndirectObjectAdjustedNovelty :: Int,
    _rOtherNovelty :: Double,
    _rOtherAdjustedNovelty :: Int,
    _rSchemaQuality :: Int,
    _rMetabolismDeltaE :: Double,
    _rChildRearingDeltaE :: Double,
    _rCoopDeltaE :: Double,
    _rAgreementDeltaE :: Double,
    _rEasementCoopDeltaE :: Double,
    _rEasementAgreementDeltaE :: Double,
    _rFlirtingDeltaE :: Double,
    _rMatingDeltaE :: Double,
    _rOtherMatingDeltaE :: Double,
    _rOtherAgreementDeltaE :: Double,
    _rOtherEasementAgreementDeltaE :: Double,
    _rNetSubjectDeltaE :: Double,
    _rNetDeltaE :: Double,
    _rErr :: Double,
    _rBirthCount :: Int,
    _rWeanCount :: Int,
    _rCooperateCount :: Int,
    _rAgreeCount :: Int,
    _rFlirtCount :: Int,
    _rMateCount :: Int,
    _rIgnoreCount :: Int,
    _rDeathCount :: Int
  }
makeLenses ''Summary

initSummary :: Int -> Summary
initSummary p = Summary
  {
    _rPopSize = p,
    _rDirectObjectNovelty = 0,
    _rDirectObjectAdjustedNovelty = 0,
    _rIndirectObjectNovelty = 0,
    _rIndirectObjectAdjustedNovelty = 0,
    _rOtherNovelty = 0,
    _rOtherAdjustedNovelty = 0,
    _rSchemaQuality = 0,
    _rMetabolismDeltaE = 0,
    _rChildRearingDeltaE = 0,
    _rCoopDeltaE = 0,
    _rAgreementDeltaE = 0,
    _rEasementCoopDeltaE = 0,
    _rEasementAgreementDeltaE = 0,
    _rFlirtingDeltaE = 0,
    _rMatingDeltaE = 0,
    _rOtherMatingDeltaE = 0,
    _rOtherAgreementDeltaE = 0,
    _rOtherEasementAgreementDeltaE = 0,
    _rNetSubjectDeltaE = 0,
    _rNetDeltaE = 0,
    _rErr = 0,
    _rBirthCount = 0,
    _rWeanCount = 0,
    _rCooperateCount = 0,
    _rAgreeCount = 0,
    _rFlirtCount = 0,
    _rMateCount = 0,
    _rIgnoreCount = 0,
    _rDeathCount = 0
  }

summaryStats :: Summary -> [Stats.Statistic]
summaryStats r =
  [
    Stats.uiStat "pop. size" (view rPopSize r),
    Stats.uiStat "DO novelty" (view rDirectObjectNovelty r),
    Stats.iStat "DO novelty (adj.)"
      (view rDirectObjectAdjustedNovelty r),
    Stats.uiStat "IO novelty" (view rIndirectObjectNovelty r),
    Stats.iStat "IO novelty (adj.)"
      (view rIndirectObjectAdjustedNovelty r),
    Stats.uiStat "novelty to other" (view rOtherNovelty r),
    Stats.iStat "novelty to other (adj.)"
      (view rOtherAdjustedNovelty r),
    Stats.iStat "SQ" (view rSchemaQuality r),
    Stats.uiStat "metabolism Δe" (view rMetabolismDeltaE r),
    Stats.uiStat "child rearing Δe" (view rChildRearingDeltaE r),
    Stats.uiStat "cooperation Δe" (view rCoopDeltaE r),
    Stats.uiStat "agreement Δe" (view rAgreementDeltaE r),
    Stats.uiStat "easement cooperation Δe" (view rEasementCoopDeltaE r),
    Stats.uiStat "easement agreement Δe"
      (view rEasementAgreementDeltaE r),
    Stats.uiStat "flirting Δe" (view rFlirtingDeltaE r),
    Stats.uiStat "mating Δe" (view rMatingDeltaE r),
    Stats.uiStat "subject net Δe" (view rNetSubjectDeltaE r),
    Stats.uiStat "other mating Δe" (view rOtherMatingDeltaE r),
    Stats.uiStat "other agreement Δe" (view rOtherAgreementDeltaE r),
    Stats.uiStat "other easement agreement Δe"
      (view rOtherEasementAgreementDeltaE r),
    Stats.uiStat "net Δe" (view rNetDeltaE r),
    Stats.uiStat "err" (view rErr r),
    Stats.iStat "bore" (view rBirthCount r),
    Stats.iStat "weaned" (view rWeanCount r),
    Stats.iStat "co-operated" (view rCooperateCount r),
    Stats.iStat "agreed" (view rAgreeCount r),
    Stats.iStat "flirted" (view rFlirtCount r),
    Stats.iStat "mated" (view rMateCount r),
    Stats.iStat "ignored" (view rIgnoreCount r),
    Stats.iStat "died" (view rDeathCount r)
  ]

data Experiment = Experiment
  {
    _subject :: ImageWain,
    _directObject :: Object,
    _indirectObject :: Object,
    _weanlings :: [ImageWain],
    _universe :: U.Universe ImageWain,
    _summary :: Summary
  }
makeLenses ''Experiment

run :: U.Universe ImageWain -> [ImageWain]
      -> StateT (U.Universe ImageWain) IO [ImageWain]
run u (me:xs) = do
  when (null xs) $ U.writeToLog "WARNING: Last wain standing!"
  (x, y) <- chooseObjects xs (U.uImageDB u)
  p <- U.popSize
  let e = Experiment { _subject = me,
                       _directObject = x,
                       _indirectObject = y,
                       _weanlings = [],
                       _universe = u,
                       _summary = initSummary p}
  e' <- liftIO $ execStateT run' e
  let modifiedAgents = addIfAgent (view directObject e')
        . addIfAgent (view indirectObject e')
            $ (view subject e'):(view weanlings e')
  U.writeToLog $
    "Modified agents: " ++ show (map agentId modifiedAgents)
  return modifiedAgents
run _ _ = error "no more wains"

run' :: StateT Experiment IO ()
run' = do
  a <- use subject
  withUniverse . U.writeToLog $ "---------- " ++ agentId a
    ++ "'s turn ----------"
  withUniverse . U.writeToLog $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  -- forage
  (imgLabel, nov, r) <- chooseAction'
  runAction (action r) imgLabel nov
  letSubjectReflect r
  adjustSubjectPassion
  when (hasLitter a) applyChildrearingCost
  weanChildren
  applyMetabolismCost
  incSubjectAge
  a' <- use subject
  withUniverse . U.writeToLog $ "End of " ++ agentId a ++ "'s turn"
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  assign (summary.rSchemaQuality) (schemaQuality a')
  when (energy a' < 0) $ assign (summary.rDeathCount) 1
  sf <- fmap U.uStatsFile $ use universe
  agentStats <- fmap ((Stats.stats a' ++) . summaryStats . fillInSummary)
                 $ use summary
  withUniverse . U.writeToLog $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  withUniverse $ updateStats agentStats sf
  rsf <- fmap U.uRawStatsFile $ use universe
  withUniverse $ writeRawStats (agentId a) rsf agentStats

fillInSummary :: Summary -> Summary
fillInSummary s = s
  {
    _rNetSubjectDeltaE = myDeltaE,
    _rNetDeltaE = myDeltaE + otherDeltaE
  }
  where myDeltaE = _rMetabolismDeltaE s
          + _rChildRearingDeltaE s
          + _rCoopDeltaE s
          + _rAgreementDeltaE s
          + _rEasementCoopDeltaE s
          + _rEasementAgreementDeltaE s
          + _rFlirtingDeltaE s
          + _rMatingDeltaE s
        otherDeltaE = _rOtherMatingDeltaE s
          + _rOtherAgreementDeltaE s
          + _rOtherEasementAgreementDeltaE s

applyMetabolismCost :: StateT Experiment IO ()
applyMetabolismCost = do
  a <- use subject
  bms <- fmap U.uBaseMetabolismDeltaE $ use universe
  cps <- fmap U.uEnergyCostPerByte $ use universe
  let deltaE = metabolismCost bms cps a
  adjustSubjectEnergy deltaE rMetabolismDeltaE "metabolism"

applyChildrearingCost :: StateT Experiment IO ()
applyChildrearingCost = do
  a <- use subject
  ccf <- fmap U.uChildCostFactor $ use universe
  bms <- fmap U.uBaseMetabolismDeltaE $ use universe
  cps <- fmap U.uEnergyCostPerByte $ use universe
  let deltaE = childRearingCost bms cps ccf a
  adjustSubjectEnergy deltaE rChildRearingDeltaE "child rearing"

metabolismCost :: Double -> Double -> ImageWain -> Double
metabolismCost b f a = b + f*s
  where s = fromIntegral (size a)

childRearingCost :: Double -> Double -> Double -> ImageWain -> Double
childRearingCost b f x a = x * (sum . map g $ litter a)
    where g c = metabolismCost b f c

schemaQuality :: ImageWain -> Int
schemaQuality
  = SQ.discrimination . elems . counterMap . classifier . brain

chooseAction' :: StateT Experiment IO (Label, Double, Response Action)
chooseAction' = do
  a <- use subject
  dObj <- use directObject
  iObj <- use indirectObject
  withUniverse . U.writeToLog $ agentId a ++ " sees " ++ objectId dObj
    ++ " and " ++ objectId iObj
  (dObjLabel, dObjNovelty, dObjNoveltyAdj,
    iObjLabel, iObjNovelty, iObjNoveltyAdj, r, a')
    <- withUniverse $
        chooseAction (objectAppearance dObj) (objectAppearance iObj) a
  assign (summary.rDirectObjectNovelty) dObjNovelty
  assign (summary.rDirectObjectAdjustedNovelty) dObjNoveltyAdj
  assign (summary.rIndirectObjectNovelty) iObjNovelty
  assign (summary.rIndirectObjectAdjustedNovelty) iObjNoveltyAdj
  withUniverse . U.writeToLog $ "To " ++ agentId a ++ ", "
    ++ objectId dObj ++ " has adjusted novelty " ++ show dObjNoveltyAdj
  withUniverse . U.writeToLog $ "To " ++ agentId a ++ ", "
    ++ objectId iObj ++ " has adjusted novelty " ++ show iObjNoveltyAdj
  withUniverse . U.writeToLog $ agentId a ++ " labels "
    ++ objectId dObj ++ " as " ++ show dObjLabel
    ++ ", " ++ objectId iObj ++ " as " ++ show iObjLabel
    ++ ", and chooses to "
    ++ describe (objectId dObj) (objectId iObj) (action r)
  assign subject a'
  return (dObjLabel, dObjNovelty, r)

incSubjectAge :: StateT Experiment IO ()
incSubjectAge = do
  a <- use subject
  a' <- withUniverse (incAge a)
  assign subject a'

describe :: String -> String -> Action -> String
describe _ iObj Cooperate = "share that classification with " ++ iObj
describe _ _    Flirt = "flirt"
describe _ _    Ignore = "do nothing"

chooseObjects :: [ImageWain] -> ImageDB -> StateT u IO (Object, Object)
chooseObjects xs db = do
  -- withUniverse . U.writeToLog $ "Direct object = " ++ objectId x
  -- withUniverse . U.writeToLog $ "Indirect object = " ++ objectId y
  (x:y:_) <- liftIO . randomlyInsertImages db . map AObject $ xs
  return (x, y)

runAction :: Action -> Label -> Double -> StateT Experiment IO ()

--
-- Co-operate
--
runAction Cooperate aLabel noveltyToMe = do
  applyCooperationEffects
  applyEarlyCooperationEffects
  a <- use subject
  dObj <- use directObject
  iObj <- use indirectObject
  case iObj of
    AObject b   -> do
      withUniverse . U.writeToLog $ agentId a ++ " tells " ++ agentId b
        ++ " that image " ++ objectId dObj ++ " has label "
        ++ show aLabel
      let (bLabel, noveltyToOther, adjustedNoveltyToOther, b')
            = classify (objectAppearance dObj) b
      assign (summary.rOtherNovelty) noveltyToOther
      assign (summary.rOtherAdjustedNovelty) adjustedNoveltyToOther
      assign indirectObject (AObject b')
      if aLabel == bLabel
        then agree aLabel noveltyToMe noveltyToOther
        else disagree aLabel bLabel
    IObject _ _ -> return ()
  
--
-- Flirt
--
runAction Flirt _ _ = do
  applyFlirtationEffects
  a <- use subject
  dObj <- use directObject
  withUniverse . U.writeToLog $
    agentId a ++ " flirts with " ++ objectId dObj
  if isImage dObj
    then return ()
    else flirt

--
-- Ignore
--
runAction Ignore _ _ = do
  a <- use subject
  dObj <- use directObject
  withUniverse . U.writeToLog $
    agentId a ++ " ignores " ++ objectId dObj
  (summary.rIgnoreCount) += 1

--
-- Utility functions
--

agree :: Label -> Double -> Double -> StateT Experiment IO ()
agree label noveltyToMe noveltyToOther = do
  a <- use subject
  dObj <- use directObject
  (AObject b) <- use indirectObject
  let dObjApp = objectAppearance dObj
  withUniverse . U.writeToLog $ agentId b ++ " agrees with "
    ++  agentId a ++ " that " ++ objectId dObj ++ " has label "
    ++ show label
  a' <- withUniverse $ teachLabel dObjApp label a -- reinforce
  b' <- withUniverse $ teachLabel dObjApp label b -- reinforce
  assign subject a'
  assign indirectObject (AObject b')
  applyAgreementEffects noveltyToMe noveltyToOther
  applyEarlyAgreementEffects

-- TODO: factor out common code in agree, disagree
  
disagree :: Label -> Label -> StateT Experiment IO ()
disagree aLabel bLabel = do
  a <- use subject
  dObj <- use directObject
  (AObject b) <- use indirectObject
  let dObjApp = objectAppearance dObj
  withUniverse . U.writeToLog $ agentId b ++ " disagrees with "
    ++ agentId a ++ ", says that " ++ objectId dObj ++ " has label "
    ++ show bLabel
  if schemaQuality a > schemaQuality b
    then do
      b' <- withUniverse $ teachLabel dObjApp aLabel b
      assign indirectObject (AObject b')
    else do
      a' <- withUniverse $ teachLabel dObjApp bLabel a
      assign subject a'

applyCooperationEffects :: StateT Experiment IO ()
applyCooperationEffects = do
  deltaE <- fmap U.uCooperationDeltaE $ use universe
  adjustSubjectEnergy deltaE rCoopDeltaE "cooperation"
  (summary.rCooperateCount) += 1

applyEarlyCooperationEffects :: StateT Experiment IO ()
applyEarlyCooperationEffects = do
  t0 <- fmap (fromIntegral . U.uEasementTime) $ use universe
  t <- fmap fromIntegral $ withUniverse U.currentTime
  when (t < t0) $ do
    eab <- fmap U.uEasementCooperationDeltaE $ use universe
    let bonus = eab*(t0 - t)/t0
    let reason = "early cooperation bonus"
    adjustSubjectEnergy bonus rEasementCoopDeltaE reason

applyAgreementEffects :: Double -> Double -> StateT Experiment IO ()
applyAgreementEffects noveltyToMe noveltyToOther = do
  x <- fmap U.uNoveltyBasedAgreementDeltaE $ use universe
  x0 <- fmap U.uMinAgreementDeltaE $ use universe
  let reason = "agreement"
  let ra = x0 + x * noveltyToMe
  adjustSubjectEnergy ra rAgreementDeltaE reason
  let rb = x0 + x * noveltyToOther
  adjustObjectEnergy indirectObject rb rOtherAgreementDeltaE reason
  (summary.rAgreeCount) += 1

-- | The first generation of wains gets a bonus to buy them some time
--   to learn about the universe.
applyEarlyAgreementEffects :: StateT Experiment IO ()
applyEarlyAgreementEffects = do
  t0 <- fmap (fromIntegral . U.uEasementTime) $ use universe
  t <- fmap fromIntegral $ withUniverse U.currentTime
  when (t < t0) $ do
    eab <- fmap U.uEasementAgreementDeltaE $ use universe
    let bonus = eab*(t0 - t)/t0
    let reason = "early agreement bonus"
    adjustSubjectEnergy bonus rEasementAgreementDeltaE reason
    adjustObjectEnergy indirectObject bonus
      rOtherEasementAgreementDeltaE reason

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  (AObject b) <- use directObject
  (a':b':_, mated) <- withUniverse (tryMating a b)
  if mated
    then do
      assign subject a'
      assign directObject (AObject b')
      recordBirths
      applyMatingEffects (energy a' - energy a) (energy b' - energy b)
    else return ()

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (litter a)

applyFlirtationEffects :: StateT Experiment IO ()
applyFlirtationEffects = do
  deltaE <- fmap U.uFlirtingDeltaE $ use universe
  adjustSubjectEnergy deltaE rFlirtingDeltaE "flirting"
  (summary.rFlirtCount) += 1

applyMatingEffects :: Double -> Double -> StateT Experiment IO ()
applyMatingEffects e1 e2 = do
  (summary . rMatingDeltaE) += e1
  (summary . rOtherMatingDeltaE) += e2
  (summary.rMateCount) += 1

weanChildren :: StateT Experiment IO ()
weanChildren = do
  (a:as) <- use subject >>= withUniverse . weanMatureChildren
  assign subject a
  assign weanlings as
  (summary.rWeanCount) += length as

withUniverse
  :: Monad m => StateT (U.Universe ImageWain) m a -> StateT Experiment m a
withUniverse f = do
  e <- get
  stateMap (\u -> set universe u e) (view universe) f

finishRound :: FilePath -> StateT (U.Universe ImageWain) IO ()
finishRound f = do
  xss <- readStats f
  let yss = summarise xss
  printStats yss
  checkStats . concat $ yss
  clearStats f

printStats :: [[Stats.Statistic]] -> StateT (U.Universe ImageWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

checkStats :: [Stats.Statistic] -> StateT (U.Universe ImageWain) IO ()
checkStats xs = do
  t <- U.currentTime
  enforceMin "avg. energy" U.uMinAvgEnergy xs "low energy"
  enforceMin "avg. classifier IQ" U.uMinAvgClassifierIQ xs
      "small classifiers"
  enforceMin "avg. decider IQ" U.uMinAvgDeciderIQ xs
      "small deciders"
  enforceMin "avg. net Δe" U.uMinAvgNetDeltaE xs
    "losing energy too quickly"
  enforceMin "avg. co-operated" U.uMinAvgCooperation xs
    "not co-operating often"
  tE <- gets U.uEasementTime
  when (t >= tE) $ do
    enforceMin "avg. age" U.uPhase1MinAvgAge xs "young population"
    enforceMin "avg. SQ" U.uPhase1MinAvgSQ xs "low SQ"
    enforceMin "avg. agreed" U.uPhase1MinAvgAgreed xs
      "not agreeing often"
    enforceMin "avg. flirted" U.uMinAvgFlirted xs
      "not flirting often"

enforceMin
  :: String -> (U.Universe ImageWain -> Double) -> [Stats.Statistic]
    -> String -> StateT (U.Universe ImageWain) IO ()
enforceMin key limit xs message = do
  x <- gets limit
  checkStat key xs (< x) message

-- enforceMax
--   :: String -> (U.Universe ImageWain -> Double) -> [Stats.Statistic]
--     -> String -> StateT (U.Universe ImageWain) IO ()
-- enforceMax key limit xs message = do
--   x <- gets limit
--   checkStat key xs (> x) message

checkStat
  :: String -> [Stats.Statistic] -> (Double -> Bool) -> String
    -> StateT (U.Universe ImageWain) IO ()
checkStat key xs f message =
  case lookup key xs of
    Just x -> when (f x) $ requestShutdown 
               (message ++ ' ':show key ++ "=" ++ show x)
    Nothing -> requestShutdown $ "Cannot find statistic: " ++ key

adjustSubjectEnergy
  :: Double -> Simple Lens Summary Double -> String
    -> StateT Experiment IO ()
adjustSubjectEnergy deltaE selector reason = do
  x <- use subject
  let before = energy x
  deltaE' <- adjustedDeltaE deltaE (1 - before)
  -- assign (summary . selector) deltaE'
  (summary . selector) += deltaE'
  assign subject (adjustEnergy deltaE' x)
  after <- fmap energy $ use subject
  withUniverse . U.writeToLog $ "Adjusting energy of " ++ agentId x
    ++ " because of " ++ reason
    ++ ". " ++ printf "%.3f" before ++ " + " ++ printf "%.3f" deltaE'
    ++ " = " ++ printf "%.3f" after

adjustObjectEnergy
  :: Simple Lens Experiment Object -> Double
    -> Simple Lens Summary Double -> String -> StateT Experiment IO ()
adjustObjectEnergy objectSelector deltaE statSelector reason = do
  x <- use objectSelector
  case x of
    AObject a -> do
      let before = energy a
      deltaE' <- adjustedDeltaE deltaE (1 - before)
      (summary . statSelector) += deltaE'
      let a' = adjustEnergy deltaE' a
      let after = energy a'
      assign objectSelector (AObject a')
      withUniverse . U.writeToLog $ "Adjusting energy of " ++ agentId a
        ++ " because of " ++ reason
        ++ ". " ++ printf "%.3f" before ++ " + "
        ++ printf "%.3f" deltaE' ++ " = " ++ printf "%.3f" after
    IObject _ _ -> return ()

adjustedDeltaE
  :: Double -> Double -> StateT Experiment IO Double
adjustedDeltaE deltaE headroom =
  if deltaE <= 0
    then return deltaE
    else do
      deltaE' <- withUniverse (U.withdrawEnergy $ min deltaE headroom)
      when (deltaE' < deltaE) $ do
        withUniverse . U.writeToLog $ "Energy pool exhausted, only "
          ++ show deltaE' ++ " available"
      return deltaE'

adjustSubjectPassion
  :: StateT Experiment IO ()
adjustSubjectPassion = do
  x <- use subject
  assign subject (adjustPassion x)

letSubjectReflect
  :: Response Action -> StateT Experiment IO ()
letSubjectReflect r = do
  x <- use subject
  p1 <- fmap objectAppearance $ use directObject
  p2 <- fmap objectAppearance $ use indirectObject
  (x', err) <- withUniverse (reflect p1 p2 r x)
  assign subject x'
  assign (summary . rErr) err

writeRawStats
  :: String -> FilePath -> [Stats.Statistic]
    -> StateT (U.Universe ImageWain) IO ()
writeRawStats n f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  t <- U.currentTime
  liftIO . appendFile f $
    "time=" ++ show t ++ ",agent=" ++ n ++ ',':raw xs ++ "\n"
