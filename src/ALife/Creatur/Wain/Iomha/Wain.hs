------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.Wain
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.Iomha.Wain
  (
    ImageWain,
    ImageTweaker(..),
    run,
    randomImageWain,
    finishRound,
    schemaQuality,
    printStats,
    idealPopControlDeltaE -- exported for testing only
  ) where

import ALife.Creatur (agentId, isAlive)
import ALife.Creatur.Counter (current, increment)
import ALife.Creatur.Task (checkPopSize)
import ALife.Creatur.Wain (Wain, Label, buildWainAndGenerateGenome,
  appearance, name, chooseAction, incAge, applyMetabolismCost,
  weanMatureChildren, pruneDeadChildren, adjustEnergy, adjustPassion,
  reflect, mate, litter, brain, energy, passion, childEnergy, age,
  imprint, wainSize)
import ALife.Creatur.Wain.Brain (Brain, classifier, predictor,
  decisionQuality, makeBrain)
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import ALife.Creatur.Wain.Classifier(buildClassifier)
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Predictor(buildPredictor)
import ALife.Creatur.Wain.GeneticSOM (RandomExponentialParams(..),
  GeneticSOM, randomExponential, schemaQuality, modelMap)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, action, outcome,
  scenario)
import ALife.Creatur.Wain.PlusMinusOne (pm1ToDouble)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import ALife.Creatur.Wain.Util (unitInterval)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Creatur.Wain.Iomha.Action (Action(..))
import qualified ALife.Creatur.Wain.Iomha.FMRI as F
import ALife.Creatur.Wain.Iomha.Image (Image, bigX)
import ALife.Creatur.Wain.Iomha.ImageTweaker (ImageTweaker(..))
import ALife.Creatur.Wain.Iomha.ImageDB (ImageDB, anyImage)
import qualified ALife.Creatur.Wain.Iomha.Universe as U
import ALife.Creatur.Persistent (getPS, putPS)
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats)
import ALife.Creatur.Wain.Statistics (summarise)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Conditional (whenM)
import Control.Lens hiding (universe)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandom, getRandomR,
  getRandomRs, getRandoms, evalRandIO, fromList)
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT, get)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Word (Word16)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
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
objectAppearance (AObject a) = view appearance a

objectEnergy :: Object -> UIDouble
objectEnergy (IObject _ _) = 0
objectEnergy (AObject a) = view energy a

objectChildEnergy :: Object -> UIDouble
objectChildEnergy (IObject _ _) = 0
objectChildEnergy (AObject a) = childEnergy a

addIfAgent :: Object -> [ImageWain] -> [ImageWain]
addIfAgent (IObject _ _) xs = xs
addIfAgent (AObject a) xs = a:xs

type ImageWain = Wain Image ImageTweaker  Action

randomImageWain
  :: RandomGen r
    => String -> U.Universe ImageWain -> Word16 -> Word16
      -> Rand r ImageWain
randomImageWain wName u classifierSize predictorSize = do
  let w = view U.uImageWidth u
  let h = view U.uImageHeight u
  let fcp = RandomExponentialParams
               { _r0Range = view U.uClassifierR0Range u,
                 _dRange = view U.uClassifierDRange u }
  fc <- randomExponential fcp
  classifierThreshold <- getRandomR (view U.uClassifierThresholdRange u)
  let c = buildClassifier fc classifierSize classifierThreshold
            ImageTweaker
  let fdp = RandomExponentialParams
              { _r0Range = view U.uPredictorR0Range u,
                _dRange = view U.uPredictorDRange u }
  fd <- randomExponential fdp
  predictorThreshold <- getRandomR (view U.uPredictorThresholdRange u)
  cw <- (makeWeights . take 3) <$> getRandoms
  rw <- (makeWeights . take 2) <$> getRandoms
  let dr = buildPredictor fd predictorSize predictorThreshold cw rw
  hw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  dOut <- getRandomR $ view U.uDefaultOutcomeRange u
  dp <- getRandomR $ view U.uDepthRange u
  let mr = makeMuser dOut dp
  let wBrain = makeBrain c mr dr hw
  wDevotion <- getRandomR . view U.uDevotionRange $ u
  wAgeOfMaturity <- getRandomR . view U.uMaturityRange $ u
  wPassionDelta <- getRandom
  let wAppearance = bigX w h
  return $ buildWainAndGenerateGenome wName wAppearance wBrain wDevotion
    wAgeOfMaturity wPassionDelta

data Summary = Summary
  {
    _rPopSize :: Int,
    _rDirectObjectNovelty :: UIDouble,
    _rDirectObjectAdjustedNovelty :: Int,
    _rIndirectObjectNovelty :: UIDouble,
    _rIndirectObjectAdjustedNovelty :: Int,
    _rOtherNovelty :: UIDouble,
    _rOtherAdjustedNovelty :: Int,
    _rMetabolismDeltaE :: Double,
    _rChildMetabolismDeltaE :: Double,
    _rCSQDeltaE :: Double,
    _rChildCSQDeltaE :: Double,
    _rDSQDeltaE :: Double,
    _rChildDSQDeltaE :: Double,
    _rDQDeltaE :: Double,
    _rChildDQDeltaE :: Double,
    _rPopControlDeltaE :: Double,
    _rChildPopControlDeltaE :: Double,
    _rCoopDeltaE :: Double,
    _rChildCoopDeltaE :: Double,
    _rAgreementDeltaE :: Double,
    _rChildAgreementDeltaE :: Double,
    _rFlirtingDeltaE :: Double,
    _rMatingDeltaE :: Double,
    _rOldAgeDeltaE :: Double,
    _rChildOldAgeDeltaE :: Double,
    _rOtherMatingDeltaE :: Double,
    _rOtherAgreementDeltaE :: Double,
    _rOtherChildAgreementDeltaE :: Double,
    _rNetDeltaE :: Double,
    _rChildNetDeltaE :: Double,
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
    _rMetabolismDeltaE = 0,
    _rChildMetabolismDeltaE = 0,
    _rCSQDeltaE = 0,
    _rChildCSQDeltaE = 0,
    _rDSQDeltaE = 0,
    _rChildDSQDeltaE = 0,
    _rDQDeltaE = 0,
    _rChildDQDeltaE = 0,
    _rPopControlDeltaE = 0,
    _rChildPopControlDeltaE = 0,
    _rCoopDeltaE = 0,
    _rChildCoopDeltaE = 0,
    _rAgreementDeltaE = 0,
    _rChildAgreementDeltaE = 0,
    _rFlirtingDeltaE = 0,
    _rMatingDeltaE = 0,
    _rOldAgeDeltaE = 0,
    _rChildOldAgeDeltaE = 0,
    _rOtherMatingDeltaE = 0,
    _rOtherAgreementDeltaE = 0,
    _rOtherChildAgreementDeltaE = 0,
    _rNetDeltaE = 0,
    _rChildNetDeltaE = 0,
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
    Stats.dStat "pop. size" (view rPopSize r),
    Stats.dStat "DO novelty" (view rDirectObjectNovelty r),
    Stats.iStat "DO novelty (adj.)"
      (view rDirectObjectAdjustedNovelty r),
    Stats.dStat "IO novelty" (view rIndirectObjectNovelty r),
    Stats.iStat "IO novelty (adj.)"
      (view rIndirectObjectAdjustedNovelty r),
    Stats.dStat "novelty to other" (view rOtherNovelty r),
    Stats.iStat "novelty to other (adj.)"
      (view rOtherAdjustedNovelty r),
    Stats.dStat "adult metabolism Δe" (view rMetabolismDeltaE r),
    Stats.dStat "child metabolism Δe" (view rChildMetabolismDeltaE r),
    Stats.dStat "adult CSQ Δe" (view rCSQDeltaE r),
    Stats.dStat "child CSQ Δe" (view rChildCSQDeltaE r),
    Stats.dStat "adult DSQ Δe" (view rDSQDeltaE r),
    Stats.dStat "child DSQ Δe" (view rChildDSQDeltaE r),
    Stats.dStat "adult DQ Δe" (view rDQDeltaE r),
    Stats.dStat "child DQ Δe" (view rChildDQDeltaE r),
    Stats.dStat "adult pop. control Δe" (view rPopControlDeltaE r),
    Stats.dStat "child pop. control Δe" (view rChildPopControlDeltaE r),
    Stats.dStat "adult cooperation Δe" (view rCoopDeltaE r),
    Stats.dStat "child cooperation Δe" (view rChildCoopDeltaE r),
    Stats.dStat "adult agreement Δe" (view rAgreementDeltaE r),
    Stats.dStat "child agreement Δe" (view rChildAgreementDeltaE r),
    Stats.dStat "adult flirting Δe" (view rFlirtingDeltaE r),
    Stats.dStat "adult mating Δe" (view rMatingDeltaE r),
    Stats.dStat "adult old age Δe" (view rOldAgeDeltaE r),
    Stats.dStat "child old age Δe" (view rChildOldAgeDeltaE r),
    Stats.dStat "other adult mating Δe" (view rOtherMatingDeltaE r),
    Stats.dStat "other adult agreement Δe"
      (view rOtherAgreementDeltaE r),
    Stats.dStat "other child agreement Δe"
      (view rOtherChildAgreementDeltaE r),
    Stats.dStat "adult net Δe" (view rNetDeltaE r),
    Stats.dStat "child net Δe" (view rChildNetDeltaE r),
    Stats.dStat "err" (view rErr r),
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

run :: [ImageWain] -> StateT (U.Universe ImageWain) IO [ImageWain]
run (me:w1:w2:xs) = do
  when (null xs) $ U.writeToLog "WARNING: Last wain standing!"
  u <- get
  (x, y) <- liftIO $ chooseObjects (view U.uFrequencies u) w1 w2
                       (view U.uImageDB u)
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
            $ view subject e' : view weanlings e'
  U.writeToLog $
    "Modified agents: " ++ show (map agentId modifiedAgents)
  return modifiedAgents
run _ = error "too few wains"

run' :: StateT Experiment IO ()
run' = do
  (e0, ec0) <- totalEnergy
  a <- use subject
  zoom universe . U.writeToLog $ "---------- " ++ agentId a
    ++ "'s turn ----------"
  zoom universe . U.writeToLog $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  runMetabolism
  applySQEffects classifier U.uCSQDeltaE rCSQDeltaE rChildCSQDeltaE
  applySQEffects predictor U.uDSQDeltaE rDSQDeltaE rChildDSQDeltaE
  applyDQEffects
  applyPopControl
  r <- chooseSubjectAction
  runAction (view action r)
  letSubjectReflect r
  adjustSubjectPassion
  subject %= incAge
  a' <- use subject
  zoom universe . U.writeToLog $ "End of " ++ agentId a ++ "'s turn"
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  unless (isAlive a') $ assign (summary.rDeathCount) 1
  summary %= fillInSummary
  (ef, ecf) <- totalEnergy
  balanceEnergyEquation e0 ec0 ef ecf
  updateChildren
  killIfTooOld
  agentStats <- ((Stats.stats a' ++) . summaryStats) <$> use summary
  zoom universe . U.writeToLog $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  rsf <- use (universe . U.uRawStatsFile)
  zoom universe $ writeRawStats (agentId a) rsf agentStats
  sf <- use (universe . U.uStatsFile)
  zoom universe $ updateStats agentStats sf

fillInSummary :: Summary -> Summary
fillInSummary s = s
  {
    _rNetDeltaE = _rMetabolismDeltaE s
         + _rCSQDeltaE s
         + _rDSQDeltaE s
         + _rDQDeltaE s
         + _rPopControlDeltaE s
         + _rCoopDeltaE s
         + _rAgreementDeltaE s
         + _rFlirtingDeltaE s
         + _rMatingDeltaE s
         + _rOldAgeDeltaE s
         + _rOtherMatingDeltaE s
         + _rOtherAgreementDeltaE s,
    _rChildNetDeltaE = _rChildMetabolismDeltaE s
         + _rChildCSQDeltaE s
         + _rChildDSQDeltaE s
         + _rChildDQDeltaE s
         + _rChildPopControlDeltaE s
         + _rChildCoopDeltaE s
         + _rChildAgreementDeltaE s
         + _rOtherChildAgreementDeltaE s
         + _rChildOldAgeDeltaE s
         -- include energy given to wains when they are born
         - _rMatingDeltaE s
         - _rOtherMatingDeltaE s
  }

balanceEnergyEquation
  :: Double -> Double -> Double -> Double -> StateT Experiment IO ()
balanceEnergyEquation e0 ec0 ef ecf = do
  netDeltaE1 <- use (summary . rNetDeltaE)
  let netDeltaE2 = ef - e0
  let err = abs (netDeltaE1 - netDeltaE2)
  when (err > 0.000001) $ do
    zoom universe . U.writeToLog $
      "WARNING: Adult energy equation doesn't balance"
    zoom universe . U.writeToLog $ "e0=" ++ show e0 ++ ", ef=" ++ show ef
      ++ ", netDeltaE2=" ++ show netDeltaE2
      ++ ", netDeltaE1=" ++ show netDeltaE1
      ++ ", err=" ++ show err
  childNetDeltaE1 <- use (summary . rChildNetDeltaE)
  let childNetDeltaE2 = ecf - ec0
  let childErr = abs (childNetDeltaE1 - childNetDeltaE2)
  when (childErr > 0.000001) $ do
    zoom universe . U.writeToLog $
      "WARNING: Child energy equation doesn't balance"
    zoom universe . U.writeToLog $ "ec0=" ++ show ec0
      ++ ", ecf=" ++ show ecf
      ++ ", childNetDeltaE2=" ++ show childNetDeltaE2
      ++ ", childNetDeltaE1=" ++ show childNetDeltaE1
      ++ ", childErr=" ++ show childErr

runMetabolism :: StateT Experiment IO ()
runMetabolism = do
  a <- use subject
  bms <- use (universe . U.uBaseMetabolismDeltaE)
  cps <- use (universe . U.uEnergyCostPerByte)
  ccf <- use (universe . U.uChildCostFactor)
  let (a', adultCost, childCost) = applyMetabolismCost bms cps ccf a
  zoom universe . U.writeToLog $ "bms=" ++ show bms ++ " cps=" ++ show cps ++ " adult size=" ++ show (view wainSize a) ++ " adult cost=" ++ show adultCost
  (summary . rMetabolismDeltaE) += adultCost
  (summary . rChildMetabolismDeltaE) += childCost
  assign subject a'

chooseSubjectAction
  :: StateT Experiment IO (Response Action)
chooseSubjectAction = do
  a <- use subject
  dObj <- use directObject
  iObj <- use indirectObject
  (dObjNovelty, dObjNoveltyAdj, iObjNovelty, iObjNoveltyAdj, r, a')
    <- zoom universe $ chooseAction3 a dObj iObj
  assign (summary.rDirectObjectNovelty) dObjNovelty
  assign (summary.rDirectObjectAdjustedNovelty) dObjNoveltyAdj
  assign (summary.rIndirectObjectNovelty) iObjNovelty
  assign (summary.rIndirectObjectAdjustedNovelty) iObjNoveltyAdj
  assign subject a'
  return r

choosePartnerAction
  :: StateT Experiment IO (Response Action)
choosePartnerAction = do
  a <- use subject
  dObj <- use directObject
  (AObject b) <- use indirectObject
  (dObjNovelty, dObjNoveltyAdj, _, _, r, b')
    <- zoom universe $ chooseAction3 b dObj (AObject a)
  assign (summary.rOtherNovelty) dObjNovelty
  assign (summary.rOtherAdjustedNovelty) dObjNoveltyAdj
  assign indirectObject (AObject b')
  return r

chooseAction3
  :: ImageWain -> Object -> Object
    -> StateT (U.Universe ImageWain) IO
        (UIDouble, Int, UIDouble, Int, Response Action, ImageWain)
chooseAction3 w dObj iObj = do
  U.writeToLog $ agentId w ++ " sees " ++ objectId dObj
    ++ " and " ++ objectId iObj
  whenM (use U.uShowPredictorModels) $ describeModels w
  let (dObjLabel:iObjLabel:_, lds, scenarioLabel, rls, r, w')
        = chooseAction [objectAppearance dObj, objectAppearance iObj] w
  let (dObjNovelty:iObjNovelty:_) = map (maximum . map snd) $ lds
  whenM (use U.uGenFmris) (writeFmri w)
  U.writeToLog $ "scenario=" ++ pretty (view scenario r)
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ objectId dObj ++ " has novelty " ++ show dObjNovelty
    ++ " and best fits classifier model " ++ show dObjLabel
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ objectId iObj ++ " has novelty " ++ show iObjNovelty
    ++ " and best fits classifier model " ++ show iObjLabel
  whenM (use U.uShowPredictions) $ describeOutcomes w rls
  let dObjNoveltyAdj
        = round $ uiToDouble dObjNovelty * fromIntegral (view age w)
  let iObjNoveltyAdj
        = round $ uiToDouble iObjNovelty * fromIntegral (view age w)
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ objectId dObj ++ " has adjusted novelty " ++ show dObjNoveltyAdj
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ objectId iObj ++ " has adjusted novelty " ++ show iObjNoveltyAdj
  U.writeToLog $ agentId w ++ " sees " ++ objectId dObj
    ++ " and chooses to " ++ show (view action r)
    ++ " based on response model " ++ show scenarioLabel
  -- let modelsBefore = models $ view (brain . classifier) w
  -- let modelsAfter = models $ view (brain . classifier) w'
  -- U.writeToLog $ "DEBUG classifier model changes = "
  --   ++ show (modelChanges modelsBefore modelsAfter)
  return (dObjNovelty, dObjNoveltyAdj, iObjNovelty, iObjNoveltyAdj, r, w')

-- modelChanges :: Eq a => [a] -> [a] -> [Int]
-- modelChanges as bs =
--   map fst . filter snd . zip [0..] $ zipWith (/=) as bs

writeFmri :: ImageWain -> StateT (U.Universe ImageWain) IO ()
writeFmri w = do
  t <- U.currentTime
  k <- zoom U.uFmriCounter current
  zoom U.uFmriCounter increment
  d <- use U.uFmriDir
  let f = d ++ "/" ++ view name w ++ '_' : show t ++ "_" ++ show k ++ ".png"
  U.writeToLog $ "Writing FMRI to " ++ f
  liftIO . F.writeFmri w $ f

describeModels :: ImageWain -> StateT (U.Universe ImageWain) IO ()
describeModels w = mapM_ (U.writeToLog . f) ms
  where ms = M.toList . modelMap . view predictor $ view brain w
        f (l, r) = view name w ++ "'s predictor model " ++ show l ++ "="
                     ++ pretty r

describeOutcomes
  :: ImageWain -> [(Response Action, Label)]
    -> StateT (U.Universe ImageWain) IO ()
describeOutcomes w = mapM_ (U.writeToLog . f)
  where f (r, l) = view name w ++ "'s predicted outcome of "
                     ++ show (view action r) ++ " is "
                     ++ (printf "%.3f" . pm1ToDouble . view outcome $ r)
                     ++ " from model " ++ show l

chooseObjects
  :: [Rational] -> ImageWain -> ImageWain -> ImageDB
    -> IO (Object, Object)
chooseObjects freqs w1 w2 db = do
  (img1, imageId1) <- evalStateT anyImage db
  (img2, imageId2) <- evalStateT anyImage db
  choosePair freqs (IObject img1 imageId1, IObject img2 imageId2)
    (AObject w1, AObject w2)

choosePair :: [Rational] -> (a, a) -> (a, a) -> IO (a, a)
choosePair freqs (i1, i2) (w1, w2)
  = fromList $ zip [(i1, i2), (i1, w1), (w1, i1), (w1, w2)] freqs

runAction :: Action -> StateT Experiment IO ()

--
-- Flirt
--
runAction Flirt = do
  applyFlirtationEffects
  a <- use subject
  dObj <- use directObject
  zoom universe . U.writeToLog $
    agentId a ++ " flirts with " ++ objectId dObj
  unless (isImage dObj) flirt

--
-- Ignore
--
runAction Ignore = do
  a <- use subject
  dObj <- use directObject
  zoom universe . U.writeToLog $
    agentId a ++ " ignores " ++ objectId dObj
  (summary.rIgnoreCount) += 1

--
-- Co-operate
--
runAction aAction = do
  applyCooperationEffects
  a <- use subject
  dObj <- use directObject
  iObj <- use indirectObject
  case iObj of
    AObject b   -> do
      zoom universe . U.writeToLog $ agentId a ++ " tells " ++ agentId b
        ++ " that image " ++ objectId dObj ++ " has label "
        ++ show aAction
      r <- choosePartnerAction
      let bAction = view action r
      if aAction == bAction
        then do
          zoom universe . U.writeToLog $ agentId b ++ " agrees with "
            ++  agentId a ++ " that " ++ objectId dObj
            ++ " has label " ++ show aAction
          applyAgreementEffects
        else do
          zoom universe . U.writeToLog $ agentId b ++ " disagrees with "
            ++ agentId a ++ ", says that " ++ objectId dObj
            ++ " has label " ++ show bAction
          applyDisagreementEffects aAction bAction
    IObject _ _ -> do
      zoom universe . U.writeToLog $ "Attempting to co-operate with an image"
      return ()

--
-- Utility functions
--

applySQEffects
  :: Simple Lens (Brain Image ImageTweaker  Action) (GeneticSOM p t)
    -> Simple Lens (U.Universe ImageWain) Double
     -> Simple Lens Summary Double -> Simple Lens Summary Double
       -> StateT Experiment IO ()
applySQEffects component deltaESelector adultSelector childSelector = do
  aSQ <- fromIntegral . schemaQuality
          <$> use (subject . brain . component)
  x <- use (universe . deltaESelector)
  let deltaE = x*aSQ
  zoom universe . U.writeToLog $
    "aSQ=" ++ show aSQ ++ " x=" ++ show x ++ " deltaE=" ++ show deltaE
  adjustSubjectEnergy deltaE adultSelector childSelector

applyDQEffects :: StateT Experiment IO ()
applyDQEffects = do
  aDQ <- fromIntegral . decisionQuality <$> use (subject . brain)
  x <- use (universe . U.uDQDeltaE)
  let deltaE = x*aDQ
  zoom universe . U.writeToLog $
    "aDQ=" ++ show aDQ ++ " x=" ++ show x ++ " deltaE=" ++ show deltaE
  adjustSubjectEnergy deltaE rDQDeltaE rChildDQDeltaE

applyPopControl :: StateT Experiment IO ()
applyPopControl = do
  deltaE <- zoom (universe . U.uPopControlDeltaE) getPS
  adjustSubjectEnergy deltaE rPopControlDeltaE rChildPopControlDeltaE

applyCooperationEffects :: StateT Experiment IO ()
applyCooperationEffects = do
  deltaE <- use (universe . U.uCooperationDeltaE)
  adjustSubjectEnergy deltaE rCoopDeltaE rChildCoopDeltaE
  (summary.rCooperateCount) += 1

applyAgreementEffects :: StateT Experiment IO ()
applyAgreementEffects = do
  a <- use subject
  AObject b <- use indirectObject
  dObj <- use directObject
  if isImage dObj
    then do
      let aDQ = fromIntegral . decisionQuality $ view brain a
      let bDQ = fromIntegral . decisionQuality $ view brain b
      aNovelty <- uiToDouble <$> use (summary . rDirectObjectNovelty)
      bNovelty <- uiToDouble <$> use (summary . rOtherNovelty)
      xd <- use (universe . U.uDQBasedAgreementDeltaE)
      xn <- use (universe . U.uNoveltyBasedAgreementDeltaE)
      x0 <- use (universe . U.uMinAgreementDeltaE)
      let ra = x0 + xn * aNovelty + xd * aDQ
      adjustSubjectEnergy ra rAgreementDeltaE rChildAgreementDeltaE
      let rb = x0 + xn * bNovelty + xd * bDQ
      adjustObjectEnergy indirectObject rb rOtherAgreementDeltaE
        rOtherChildAgreementDeltaE
      (summary.rAgreeCount) += 1
    else
      zoom universe . U.writeToLog $
        "No reward for agreeing on a classification for a wain"

applyDisagreementEffects :: Action -> Action -> StateT Experiment IO ()
applyDisagreementEffects aAction bAction = do
  aNovelty <- uiToDouble <$> use (summary . rDirectObjectNovelty)
  bNovelty <- uiToDouble <$> use (summary . rOtherNovelty)
  a <- use subject
  AObject b <- use indirectObject
  pa <- view appearance <$> use subject
  dObj <- use directObject
  let p1 = objectAppearance dObj
  let pb = view appearance b
  -- zoom universe . U.writeToLog $ "DEBUG aNovelty=" ++ show aNovelty
  -- zoom universe . U.writeToLog $ "DEBUG bNovelty=" ++ show bNovelty
  let aConfidence = (1 - aNovelty)*(fromIntegral . view age $ a)
  let bConfidence = (1 - bNovelty)*(fromIntegral . view age $ b)
  zoom universe . U.writeToLog $
    agentId a ++ "'s confidence is " ++ printf "%.3f" aConfidence
  zoom universe . U.writeToLog $
    agentId b ++ "'s confidence is " ++ printf "%.3f" bConfidence
  whenM (use $ universe . U.uAdultAdultTeaching) $
    if aConfidence > bConfidence
      then do
        zoom universe . U.writeToLog $ view name a ++ " teaches " ++ view name b
        zoom universe . U.writeToLog $
          view name b ++ " learns from " ++ view name a ++ " that "
            ++ objectId dObj ++ " is " ++ show aAction
        assign indirectObject (AObject $ imprint [p1, pa] aAction b)
      else do
        zoom universe . U.writeToLog $
          view name a ++ " learns from " ++ view name b ++ " that "
            ++ objectId dObj ++ " is " ++ show bAction
        assign subject $ imprint [p1, pb] bAction a

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  (AObject b) <- use directObject
  babyName <- zoom universe U.genName
  (a':b':_, msgs, aMatingDeltaE, bMatingDeltaE)
    <- liftIO . evalRandIO $ mate a b babyName
  if null msgs
    then do
      zoom universe . U.writeToLog $
        agentId a ++ " and " ++ agentId b ++ " mated"
      zoom universe . U.writeToLog $ "DEBUG: after mating, a's passion is " ++ show (view passion a')
      assign subject a'
      assign directObject (AObject b')
      recordBirths
      (summary . rMatingDeltaE) += aMatingDeltaE
      (summary . rOtherMatingDeltaE) += bMatingDeltaE
      (summary . rMateCount) += 1
    else mapM_ (zoom universe . U.writeToLog) msgs
  zoom universe . U.writeToLog $ "DEBUG: end of flirt method msgs=" ++ show msgs

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (view litter a)

applyFlirtationEffects :: StateT Experiment IO ()
applyFlirtationEffects = do
  deltaE <- use (universe . U.uFlirtingDeltaE)
  adjustSubjectEnergy deltaE rFlirtingDeltaE undefined
  (summary.rFlirtCount) += 1

updateChildren :: StateT Experiment IO ()
updateChildren = do
  (a:matureChildren) <- weanMatureChildren <$> use subject
  assign subject a
  (a':deadChildren) <- pruneDeadChildren <$> use subject
  assign subject a'
  assign weanlings (matureChildren ++ deadChildren)
  (summary.rWeanCount) += length matureChildren

killIfTooOld :: StateT Experiment IO ()
killIfTooOld = do
  a <- view age <$> use subject
  maxAge <- use (universe . U.uMaxAge)
  when (fromIntegral a > maxAge) $
    adjustSubjectEnergy (-100) rOldAgeDeltaE rChildOldAgeDeltaE

finishRound :: FilePath -> StateT (U.Universe ImageWain) IO ()
finishRound f = do
  xss <- readStats f
  let yss = summarise xss
  printStats yss
  let zs = concat yss
  adjustPopControlDeltaE zs
  cs <- use U.uCheckpoints
  enforceAll zs cs
  clearStats f
  (a, b) <- use U.uPopulationAllowedRange
  checkPopSize (a, b)

adjustPopControlDeltaE
  :: [Stats.Statistic] -> StateT (U.Universe ImageWain) IO ()
adjustPopControlDeltaE xs =
  unless (null xs) $ do
    pop <- U.popSize
    U.writeToLog $ "pop=" ++ show pop
    idealPop <- use U.uIdealPopulationSize
    U.writeToLog $ "ideal pop=" ++ show idealPop

    let (Just adultNet) = Stats.lookup "avg. adult net Δe" xs
    U.writeToLog $ "adultNet=" ++ show adultNet
    let (Just childNet) = Stats.lookup "avg. child net Δe" xs
    U.writeToLog $ "childNet=" ++ show childNet

    let (Just adultPopControl)
          = Stats.lookup "avg. adult pop. control Δe" xs
    U.writeToLog $ "adultPopControl=" ++ show adultPopControl
    let (Just childPopControl)
          = Stats.lookup "avg. child pop. control Δe" xs
    U.writeToLog $ "childPopControl=" ++ show childPopControl

    let avgEnergyToBalance
          = adultNet + childNet - adultPopControl - childPopControl
    U.writeToLog $ "avgEnergyToBalance=" ++ show avgEnergyToBalance
    let c = idealPopControlDeltaE idealPop pop avgEnergyToBalance
    U.writeToLog $ "Adjusted pop. control Δe = " ++ show c
    zoom U.uPopControlDeltaE $ putPS c

idealPopControlDeltaE :: Int -> Int -> Double -> Double
idealPopControlDeltaE idealPop pop e
  | idealPop == 0 = error "idealPop == 0"
  | pop == 0      = error "pop == 0"
  | otherwise    = -f*e
  where f = if e < 0
              then fromIntegral idealPop / fromIntegral pop
              else fromIntegral pop / fromIntegral idealPop

-- lookupStat
--   :: String -> [Stats.Statistic]
--     -> StateT (U.Universe ImageWain) IO (Maybe Double)
-- lookupStat key xs = do
--   let result = Stats.lookup key xs
--   when (isNothing result && not (null xs)) $ -- ignore missing stats file
--     requestShutdown $ "Cannot find statistic: " ++ key
--   return result

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- fmap uiToDouble $ view energy <$> use subject
  b <- fmap uiToDouble $ objectEnergy <$> use directObject
  c <- fmap uiToDouble $ objectEnergy <$> use indirectObject
  d <- fmap uiToDouble $ childEnergy <$> use subject
  e <- fmap uiToDouble $ objectChildEnergy <$> use directObject
  f <- fmap uiToDouble $ objectChildEnergy <$> use indirectObject
  return (a + b + c, d + e + f)

printStats :: [[Stats.Statistic]] -> StateT (U.Universe ImageWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

adjustSubjectEnergy
  :: Double -> Simple Lens Summary Double
    -> Simple Lens Summary Double -> StateT Experiment IO ()
adjustSubjectEnergy deltaE adultSelector childSelector = do
  x <- use subject
  let (x', adultDeltaE, childDeltaE) = adjustEnergy deltaE x
  (summary . adultSelector) += adultDeltaE
  when (childDeltaE /= 0) $
    (summary . childSelector) += childDeltaE
  assign subject x'

adjustObjectEnergy
  :: Simple Lens Experiment Object -> Double
    -> Simple Lens Summary Double -> Simple Lens Summary Double
      -> StateT Experiment IO ()
adjustObjectEnergy
    objectSelector deltaE adultSelector childSelector = do
  x <- use objectSelector
  case x of
    AObject a -> do
      let (a', adultDeltaE, childDeltaE) = adjustEnergy deltaE a
      (summary . adultSelector) += adultDeltaE
      when (childDeltaE /= 0) $
        (summary . childSelector) += childDeltaE
      assign objectSelector (AObject a')
    IObject _ _ -> return ()

adjustSubjectPassion
  :: StateT Experiment IO ()
adjustSubjectPassion = subject %= adjustPassion

letSubjectReflect
  :: Response Action -> StateT Experiment IO ()
letSubjectReflect r = do
  x <- use subject
  p1 <- objectAppearance <$> use directObject
  p2 <- objectAppearance <$> use indirectObject
  let (x', err) = reflect [p1, p2] r x
  assign subject x'
  assign (summary . rErr) err
  -- let modelsBefore = models $ view (brain . predictor) x
  -- let modelsAfter = models $ view (brain . predictor) x'
  -- zoom universe . U.writeToLog $ "DEBUG predictor model changes = "
  --   ++ show (modelChanges modelsBefore modelsAfter)

writeRawStats
  :: String -> FilePath -> [Stats.Statistic]
    -> StateT (U.Universe ImageWain) IO ()
writeRawStats n f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  t <- U.currentTime
  liftIO . appendFile f $
    "time=" ++ show t ++ ",agent=" ++ n ++ ',':raw xs ++ "\n"
