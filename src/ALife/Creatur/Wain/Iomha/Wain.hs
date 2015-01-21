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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.Iomha.Wain
  (
    ImageWain,
    run,
    randomImageWain,
    finishRound,
    schemaQuality,
    printStats
  ) where

import ALife.Creatur (agentId, isAlive)
import ALife.Creatur.Task (checkPopSize)
import ALife.Creatur.Util (stateMap)
import ALife.Creatur.Wain (Wain, Label, buildWainAndGenerateGenome,
  appearance, name, chooseAction, incAge, applyMetabolismCost,
  weanMatureChildren, pruneDeadChildren, adjustEnergy, adjustPassion,
  reflect, mate, litter, brain, energy, childEnergy, age, imprint)
import ALife.Creatur.Wain.Brain (decider, Brain(..))
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import ALife.Creatur.Wain.GeneticSOM (RandomExponentialParams(..),
  randomExponential, buildGeneticSOM, numModels, schemaQuality, toList)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, randomResponse, action,
  outcome, scenario)
import qualified ALife.Creatur.Wain.Scenario as S
import ALife.Creatur.Wain.Util (unitInterval)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Creatur.Wain.Iomha.Action (Action(..))
import qualified ALife.Creatur.Wain.Iomha.FMRI as F
import ALife.Creatur.Wain.Iomha.Image (Image, stripedImage, randomImage)
import ALife.Creatur.Wain.Iomha.ImageDB (ImageDB, anyImage)
import qualified ALife.Creatur.Wain.Iomha.Universe as U
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats)
import ALife.Creatur.Wain.Statistics (summarise)
import Control.Applicative ((<$>))
import Control.Conditional (whenM)
import Control.Lens hiding (Action, universe)
import Control.Monad (replicateM, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandomR, evalRandIO)
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT, get,
  gets)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Word (Word16)
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

objectEnergy :: Object -> Double
objectEnergy (IObject _ _) = 0
objectEnergy (AObject a) = energy a

objectChildEnergy :: Object -> Double
objectChildEnergy (IObject _ _) = 0
objectChildEnergy (AObject a) = childEnergy a

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
    => String -> U.Universe ImageWain -> Word16 -> Word16 -> Rand r ImageWain
randomImageWain wainName u classifierSize deciderSize = do
  let w = U.uImageWidth u
  let h = U.uImageHeight u
  imgs <- replicateM (fromIntegral classifierSize) (randomImage w h)
  -- let imgs = replicate classifierSize $ blankImage w h
  let fcp = RandomExponentialParams
               { r0Range = U.uClassifierR0Range u,
                 dRange = U.uClassifierDRange u }
  fc <- randomExponential fcp
  let c = buildGeneticSOM fc imgs
  let fdp = RandomExponentialParams
              { r0Range = U.uDeciderR0Range u,
                dRange = U.uDeciderDRange u }
  fd <- randomExponential fdp
  xs <- replicateM (fromIntegral deciderSize) $
         randomResponse (numModels c) 
  let b = Brain c (buildGeneticSOM fd xs)
  d <- getRandomR (U.uDevotionRange u)
  m <- getRandomR (U.uMaturityRange u)
  p <- getRandomR unitInterval
  let app = stripedImage w h
  return $ buildWainAndGenerateGenome wainName app b d m p

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
    _rChildMetabolismDeltaE :: Double,
    _rCoopDeltaE :: Double,
    _rChildCoopDeltaE :: Double,
    _rAgreementDeltaE :: Double,
    _rChildAgreementDeltaE :: Double,
    _rFlirtingDeltaE :: Double,
    _rMatingDeltaE :: Double,
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
    _rSchemaQuality = 0,
    _rMetabolismDeltaE = 0,
    _rChildMetabolismDeltaE = 0,
    _rCoopDeltaE = 0,
    _rChildCoopDeltaE = 0,
    _rAgreementDeltaE = 0,
    _rChildAgreementDeltaE = 0,
    _rFlirtingDeltaE = 0,
    _rMatingDeltaE = 0,
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
    Stats.uiStat "adult metabolism Δe" (view rMetabolismDeltaE r),
    Stats.uiStat "child metabolism Δe" (view rChildMetabolismDeltaE r),
    Stats.uiStat "adult cooperation Δe" (view rCoopDeltaE r),
    Stats.uiStat "child cooperation Δe" (view rChildCoopDeltaE r),
    Stats.uiStat "adult agreement Δe" (view rAgreementDeltaE r),
    Stats.uiStat "child agreement Δe" (view rChildAgreementDeltaE r),
    Stats.uiStat "adult flirting Δe" (view rFlirtingDeltaE r),
    Stats.uiStat "adult mating Δe" (view rMatingDeltaE r),
    Stats.uiStat "other adult mating Δe" (view rOtherMatingDeltaE r),
    Stats.uiStat "other adult agreement Δe" (view rOtherAgreementDeltaE r),
    Stats.uiStat "other child agreement Δe"
      (view rOtherChildAgreementDeltaE r),
    Stats.uiStat "adult net Δe" (view rNetDeltaE r),
    Stats.uiStat "child net Δe" (view rChildNetDeltaE r),
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
            $ view subject e' : view weanlings e'
  U.writeToLog $
    "Modified agents: " ++ show (map agentId modifiedAgents)
  return modifiedAgents
run _ _ = error "no more wains"

run' :: StateT Experiment IO ()
run' = do
  (e0, ec0) <- totalEnergy
  a <- use subject
  withUniverse . U.writeToLog $ "---------- " ++ agentId a
    ++ "'s turn ----------"
  withUniverse . U.writeToLog $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  r <- chooseSubjectAction
  runAction (action r)
  letSubjectReflect r
  adjustSubjectPassion
  runMetabolism
  subject %= incAge
  a' <- use subject
  withUniverse . U.writeToLog $ "End of " ++ agentId a ++ "'s turn"
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  assign (summary.rSchemaQuality) (schemaQuality . decider . brain $ a')
  unless (isAlive a') $ assign (summary.rDeathCount) 1
  summary %= fillInSummary
  (ef, ecf) <- totalEnergy
  balanceEnergyEquation e0 ec0 ef ecf
  updateChildren
  agentStats <- ((Stats.stats a' ++) . summaryStats) <$> use summary
  withUniverse . U.writeToLog $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  rsf <- U.uRawStatsFile <$> use universe
  withUniverse $ writeRawStats (agentId a) rsf agentStats
  whenM (U.uGenFmris <$> use universe) writeFmri
  sf <- U.uStatsFile <$> use universe
  withUniverse $ updateStats agentStats sf

writeFmri :: StateT Experiment IO ()
writeFmri = do
  w <- use subject
  t <- withUniverse U.currentTime
  d <- U.uFmriDir <$> use universe
  let f = d ++ "/" ++ name w ++ '_' : show t ++ ".png"
  liftIO . F.writeFmri w $ f

fillInSummary :: Summary -> Summary
fillInSummary s = s
  {
    _rNetDeltaE = _rMetabolismDeltaE s
         + _rCoopDeltaE s
         + _rAgreementDeltaE s
         + _rFlirtingDeltaE s
         + _rMatingDeltaE s
         + _rOtherMatingDeltaE s
         + _rOtherAgreementDeltaE s, 
    _rChildNetDeltaE = _rChildMetabolismDeltaE s
         + _rChildCoopDeltaE s
         + _rChildAgreementDeltaE s
         + _rOtherChildAgreementDeltaE s
         - _rMatingDeltaE s
  }

balanceEnergyEquation
  :: Double -> Double -> Double -> Double -> StateT Experiment IO ()
balanceEnergyEquation e0 ec0 ef ecf = do
  netDeltaE1 <- use (summary . rNetDeltaE)
  let netDeltaE2 = ef - e0
  let err = abs (netDeltaE1 - netDeltaE2)
  when (err > 0.0001) $ do
    withUniverse . U.writeToLog $
      "WARNING: Adult energy equation doesn't balance"
    withUniverse . U.writeToLog $ "e0=" ++ show e0 ++ ", ef=" ++ show ef
      ++ ", netDeltaE2=" ++ show netDeltaE2
      ++ ", netDeltaE1=" ++ show netDeltaE1
      ++ ", err=" ++ show err
  childNetDeltaE1 <- use (summary . rChildNetDeltaE)
  let childNetDeltaE2 = ecf - ec0
  let childErr = abs (childNetDeltaE1 - childNetDeltaE2)
  when (childErr > 0.0001) $ do
    withUniverse . U.writeToLog $
      "WARNING: Child energy equation doesn't balance"
    withUniverse . U.writeToLog $ "ec0=" ++ show ec0
      ++ ", ecf=" ++ show ecf
      ++ ", childNetDeltaE2=" ++ show childNetDeltaE2
      ++ ", childNetDeltaE1=" ++ show childNetDeltaE1
      ++ ", childErr=" ++ show childErr

runMetabolism :: StateT Experiment IO ()
runMetabolism = do
  a <- use subject
  bms <- U.uBaseMetabolismDeltaE <$> use universe
  cps <- U.uEnergyCostPerByte <$> use universe
  ccf <- U.uChildCostFactor <$> use universe
  let (a', adultCost, childCost) = applyMetabolismCost bms cps ccf a
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
    <- withUniverse $ chooseAction3 a dObj iObj
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
    <- withUniverse $ chooseAction3 b dObj (AObject a)
  assign (summary.rOtherNovelty) dObjNovelty
  assign (summary.rOtherAdjustedNovelty) dObjNoveltyAdj
  assign indirectObject (AObject b')
  return r

chooseAction3
  :: ImageWain -> Object -> Object
    -> StateT (U.Universe ImageWain) IO
        (Double, Int, Double, Int, Response Action, ImageWain)
chooseAction3 w dObj iObj = do
  U.writeToLog $ agentId w ++ " sees " ++ objectId dObj
    ++ " and " ++ objectId iObj
  whenM (gets U.uShowDeciderModels) $
    describeModels w
  let (r, w', xs)
        = chooseAction (objectAppearance dObj) (objectAppearance iObj) w
  whenM (gets U.uShowPredictions) $
    describeOutcomes w xs
  let (dObjNovelty, dObjNoveltyAdj, iObjNovelty, iObjNoveltyAdj)
        = novelties w r
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ objectId dObj ++ " has adjusted novelty " ++ show dObjNoveltyAdj
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ objectId iObj ++ " has adjusted novelty " ++ show iObjNoveltyAdj
  U.writeToLog $ agentId w ++ " sees " ++ objectId dObj
    ++ " and chooses to "
    ++ show (action r)
  return (dObjNovelty, dObjNoveltyAdj, iObjNovelty, iObjNoveltyAdj, r, w')

novelties
  :: ImageWain -> Response Action -> (Double, Int, Double, Int)
novelties w r
  = (dObjNovelty, dObjNoveltyAdj, iObjNovelty, iObjNoveltyAdj)
  where dObjNovelty = maximum . S.directObject . scenario $ r
        iObjNovelty = maximum . S.indirectObject . scenario $ r
        dObjNoveltyAdj = round $ dObjNovelty * fromIntegral (age w)
        iObjNoveltyAdj = round $ iObjNovelty * fromIntegral (age w)

describeModels :: ImageWain -> StateT (U.Universe ImageWain) IO ()
describeModels w = mapM_ (U.writeToLog . f) ms
  where ms = toList . decider $ brain w
        f (l, r) = name w ++ "'s decider model " ++ show l ++ "="
                     ++ pretty r

describeOutcomes
  :: ImageWain -> [(Response Action, Label)]
    -> StateT (U.Universe ImageWain) IO ()
describeOutcomes w = mapM_ (U.writeToLog . f)
  where f (r, l) = name w ++ "'s predicted outcome of "
                     ++ show (action r) ++ " is "
                     ++ (printf "%.3f" . fromJust . outcome $ r)
                     ++ " from model " ++ show l

chooseObjects :: [ImageWain] -> ImageDB -> StateT u IO (Object, Object)
chooseObjects xs db = do
  -- withUniverse . U.writeToLog $ "Direct object = " ++ objectId x
  -- withUniverse . U.writeToLog $ "Indirect object = " ++ objectId y
  (x:y:_) <- liftIO . randomlyInsertImages db . map AObject $ xs
  return (x, y)

runAction :: Action -> StateT Experiment IO ()

--
-- Flirt
--
runAction Flirt = do
  applyFlirtationEffects
  a <- use subject
  dObj <- use directObject
  withUniverse . U.writeToLog $
    agentId a ++ " flirts with " ++ objectId dObj
  unless (isImage dObj) flirt

--
-- Ignore
--
runAction Ignore = do
  a <- use subject
  dObj <- use directObject
  withUniverse . U.writeToLog $
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
      withUniverse . U.writeToLog $ agentId a ++ " tells " ++ agentId b
        ++ " that image " ++ objectId dObj ++ " has label "
        ++ show aAction
      r <- choosePartnerAction
      let bAction = action r
      if aAction == bAction
        then do
          withUniverse . U.writeToLog $ agentId b ++ " agrees with "
            ++  agentId a ++ " that " ++ objectId dObj
            ++ " has label " ++ show aAction
          applyAgreementEffects
        else do
          withUniverse . U.writeToLog $ agentId b ++ " disagrees with "
            ++ agentId a ++ ", says that " ++ objectId dObj
            ++ " has label " ++ show bAction
          applyDisagreementEffects aAction bAction
    IObject _ _ -> return ()
  
--
-- Utility functions
--

applyCooperationEffects :: StateT Experiment IO ()
applyCooperationEffects = do
  deltaE <- U.uCooperationDeltaE <$> use universe
  adjustSubjectEnergy deltaE rCoopDeltaE rChildCoopDeltaE
  (summary.rCooperateCount) += 1

applyAgreementEffects :: StateT Experiment IO ()
applyAgreementEffects = do
  aNovelty <- use $ summary . rDirectObjectNovelty
  bNovelty <- use $ summary . rOtherNovelty
  x <- U.uNoveltyBasedAgreementDeltaE <$> use universe
  x0 <- U.uMinAgreementDeltaE <$> use universe
  let ra = x0 + x * aNovelty
  adjustSubjectEnergy ra rAgreementDeltaE rChildAgreementDeltaE
  let rb = x0 + x * bNovelty
  adjustObjectEnergy indirectObject rb rOtherAgreementDeltaE
    rOtherChildAgreementDeltaE
  (summary.rAgreeCount) += 1

applyDisagreementEffects :: Action -> Action -> StateT Experiment IO ()
applyDisagreementEffects aAction bAction = do
  aNovelty <- use $ summary . rDirectObjectNovelty
  bNovelty <- use $ summary . rOtherNovelty
  a <- use subject
  AObject b <- use indirectObject
  pa <- appearance <$> use subject
  p1 <- objectAppearance <$> use directObject
  let pb = appearance b
  let aConfidence = (1 - aNovelty)*(fromIntegral . age $ a)
  let bConfidence = (1 - bNovelty)*(fromIntegral . age $ b)
  withUniverse . U.writeToLog $
    "a's confidence is " ++ printf "%.3f" aConfidence
  withUniverse . U.writeToLog $
    "b's confidence is " ++ printf "%.3f" bConfidence
  if aConfidence > bConfidence
    then do
      withUniverse . U.writeToLog $ name a ++ " teaches " ++ name b
      assign indirectObject (AObject $ imprint p1 pa aAction b)
    else do
      withUniverse . U.writeToLog $ name a ++ " learns from " ++ name b
      assign subject $ imprint p1 pb bAction a
  
flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  (AObject b) <- use directObject
  babyName <- withUniverse U.genName
  (a':b':_, msgs, aMatingDeltaE, bMatingDeltaE)
    <- liftIO . evalRandIO $ mate a b babyName
  if null msgs
    then do
      assign subject a'
      assign directObject (AObject b')
      recordBirths
      (summary . rMatingDeltaE) += aMatingDeltaE
      (summary . rOtherMatingDeltaE) += bMatingDeltaE
      (summary . rMateCount) += 1
    else mapM_ (withUniverse . U.writeToLog) msgs

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (litter a)

applyFlirtationEffects :: StateT Experiment IO ()
applyFlirtationEffects = do
  deltaE <- U.uFlirtingDeltaE <$> use universe
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
  let zs = concat yss
  cs <- gets U.uCheckpoints
  enforceAll zs cs
  clearStats f
  (a, b) <- gets U.uPopulationAllowedRange
  checkPopSize (a, b)

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- energy <$> use subject
  b <- objectEnergy <$> use directObject
  c <- objectEnergy <$> use indirectObject
  d <- childEnergy <$> use subject
  e <- objectChildEnergy <$> use directObject
  f <- objectChildEnergy <$> use indirectObject
  return (a + b + c, d + e + f)

printStats :: [[Stats.Statistic]] -> StateT (U.Universe ImageWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

adjustSubjectEnergy
  :: Double -> Simple Lens Summary Double -> Simple Lens Summary Double
    -> StateT Experiment IO ()
adjustSubjectEnergy deltaE adultSelector childSelector = do
  x <- use subject
  let (x', adultDeltaE, childDeltaE) = adjustEnergy deltaE x
  (summary . adultSelector) += adultDeltaE
  when (childDeltaE /= 0) $ (summary . childSelector) += childDeltaE
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
      when (childDeltaE /= 0) $ (summary . childSelector) += childDeltaE
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
  let (x', err) = reflect p1 p2 r x
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
