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
import ALife.Creatur.Wain (Wain, buildWainAndGenerateGenome, appearance,
  name, chooseAction, incAge, applyMetabolismCost, weanMatureChildren,
  pruneDeadChildren, adjustEnergy, adjustPassion, reflect, tryMating,
  litter, brain)
import ALife.Creatur.Wain.Brain (decider, Brain(..))
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import ALife.Creatur.Wain.GeneticSOM (RandomExponentialParams(..),
  randomExponential, buildGeneticSOM, numModels, schemaQuality)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, randomResponse, action)
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
import Control.Monad.Random (Rand, RandomGen, getRandomR)
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT, get,
  gets)
import Data.List (intercalate)
import Data.Word (Word16)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import System.Random (randomIO, randomRIO)

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
  a <- use subject
  withUniverse . U.writeToLog $ "---------- " ++ agentId a
    ++ "'s turn ----------"
  withUniverse . U.writeToLog $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  (nov, r) <- chooseSubjectAction
  runAction (action r) nov
  letSubjectReflect r
  adjustSubjectPassion
  runMetabolism
  incSubjectAge
  a' <- use subject
  withUniverse . U.writeToLog $ "End of " ++ agentId a ++ "'s turn"
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  assign (summary.rSchemaQuality) (schemaQuality . decider . brain $ a')
  unless (isAlive a') $ assign (summary.rDeathCount) 1
  sf <- U.uStatsFile <$> use universe
  agentStats <- ((Stats.stats a' ++) . summaryStats . fillInSummary)
                 <$> use summary
  withUniverse . U.writeToLog $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  withUniverse $ updateStats agentStats sf
  rsf <- U.uRawStatsFile <$> use universe
  withUniverse $ writeRawStats (agentId a) rsf agentStats
  whenM (U.uGenFmris <$> use universe) writeFmri
  updateChildren

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
  }
 
runMetabolism :: StateT Experiment IO ()
runMetabolism = do
  a <- use subject
  bms <- U.uBaseMetabolismDeltaE <$> use universe
  cps <- U.uEnergyCostPerByte <$> use universe
  ccf <- U.uChildCostFactor <$> use universe
  (a', adultCost, childCost)
    <- withUniverse $ applyMetabolismCost bms cps ccf a
  (summary . rMetabolismDeltaE) += adultCost
  (summary . rChildMetabolismDeltaE) += childCost
  assign subject a'

chooseSubjectAction
  :: StateT Experiment IO (Double, Response Action)
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
  return (dObjNovelty, r)

choosePartnerAction
  :: StateT Experiment IO (Double, Response Action)
choosePartnerAction = do
  a <- use subject
  dObj <- use directObject
  (AObject b) <- use indirectObject
  (dObjNovelty, dObjNoveltyAdj, _, _, r, b')
    <- withUniverse $ chooseAction3 b dObj (AObject a)
  assign (summary.rOtherNovelty) dObjNovelty
  assign (summary.rOtherAdjustedNovelty) dObjNoveltyAdj
  assign indirectObject (AObject b')
  return (dObjNovelty, r)

chooseAction3
  :: ImageWain -> Object -> Object
    -> StateT (U.Universe ImageWain) IO
        (Double, Int, Double, Int, Response Action, ImageWain)
chooseAction3 w dObj iObj = do
  U.writeToLog $ agentId w ++ " sees " ++ objectId dObj
    ++ " and " ++ objectId iObj
  (_, _, dObjNovelty, dObjNoveltyAdj,
    _, _, iObjNovelty, iObjNoveltyAdj, r, w')
      <- chooseAction (objectAppearance dObj) (objectAppearance iObj) w
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ objectId dObj ++ " has adjusted novelty " ++ show dObjNoveltyAdj
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ objectId iObj ++ " has adjusted novelty " ++ show iObjNoveltyAdj
  U.writeToLog $ agentId w ++ " sees " ++ objectId dObj
    ++ " and chooses to "
    ++ show (action r)
  return (dObjNovelty, dObjNoveltyAdj, iObjNovelty, iObjNoveltyAdj, r, w')
  

incSubjectAge :: StateT Experiment IO ()
incSubjectAge = do
  a <- use subject
  a' <- withUniverse (incAge a)
  assign subject a'

chooseObjects :: [ImageWain] -> ImageDB -> StateT u IO (Object, Object)
chooseObjects xs db = do
  -- withUniverse . U.writeToLog $ "Direct object = " ++ objectId x
  -- withUniverse . U.writeToLog $ "Indirect object = " ++ objectId y
  (x:y:_) <- liftIO . randomlyInsertImages db . map AObject $ xs
  return (x, y)

runAction :: Action -> Double -> StateT Experiment IO ()

--
-- Flirt
--
runAction Flirt _ = do
  applyFlirtationEffects
  a <- use subject
  dObj <- use directObject
  withUniverse . U.writeToLog $
    agentId a ++ " flirts with " ++ objectId dObj
  unless (isImage dObj) flirt

--
-- Ignore
--
runAction Ignore _ = do
  a <- use subject
  dObj <- use directObject
  withUniverse . U.writeToLog $
    agentId a ++ " ignores " ++ objectId dObj
  (summary.rIgnoreCount) += 1

--
-- Co-operate
--
runAction aAction noveltyToMe = do
  applyCooperationEffects
  a <- use subject
  dObj <- use directObject
  iObj <- use indirectObject
  case iObj of
    AObject b   -> do
      withUniverse . U.writeToLog $ agentId a ++ " tells " ++ agentId b
        ++ " that image " ++ objectId dObj ++ " has label "
        ++ show aAction
      (noveltyToOther, r) <- choosePartnerAction
      let bAction = action r
      if aAction == bAction
        then do
          withUniverse . U.writeToLog $ agentId b ++ " agrees with "
            ++  agentId a ++ " that " ++ objectId dObj
            ++ " has label " ++ show aAction
          applyAgreementEffects noveltyToMe noveltyToOther
        else
          withUniverse . U.writeToLog $ agentId b ++ " disagrees with "
            ++ agentId a ++ ", says that " ++ objectId dObj
            ++ " has label " ++ show bAction
    IObject _ _ -> return ()
  
--
-- Utility functions
--

applyCooperationEffects :: StateT Experiment IO ()
applyCooperationEffects = do
  deltaE <- U.uCooperationDeltaE <$> use universe
  adjustSubjectEnergy deltaE rCoopDeltaE rChildCoopDeltaE "cooperation"
  (summary.rCooperateCount) += 1

applyAgreementEffects :: Double -> Double -> StateT Experiment IO ()
applyAgreementEffects noveltyToMe noveltyToOther = do
  x <- U.uNoveltyBasedAgreementDeltaE <$> use universe
  x0 <- U.uMinAgreementDeltaE <$> use universe
  let reason = "agreement"
  let ra = x0 + x * noveltyToMe
  adjustSubjectEnergy ra rAgreementDeltaE rChildAgreementDeltaE reason
  let rb = x0 + x * noveltyToOther
  adjustObjectEnergy indirectObject rb rOtherAgreementDeltaE
    rOtherChildAgreementDeltaE reason
  (summary.rAgreeCount) += 1

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  (AObject b) <- use directObject
  (a':b':_, mated, aMatingDeltaE, bMatingDeltaE)
    <- withUniverse (tryMating a b)
  when mated $ do
    assign subject a'
    assign directObject (AObject b')
    recordBirths
    (summary . rMatingDeltaE) += aMatingDeltaE
    (summary . rOtherMatingDeltaE) += bMatingDeltaE
    (summary . rMateCount) += 1

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (litter a)

applyFlirtationEffects :: StateT Experiment IO ()
applyFlirtationEffects = do
  deltaE <- U.uFlirtingDeltaE <$> use universe
  adjustSubjectEnergy deltaE rFlirtingDeltaE undefined "flirting"
  (summary.rFlirtCount) += 1

updateChildren :: StateT Experiment IO ()
updateChildren = do
  (a:matureChildren) <- use subject >>= withUniverse . weanMatureChildren
  assign subject a
  (a':deadChildren) <- use subject >>= withUniverse . pruneDeadChildren
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


printStats :: [[Stats.Statistic]] -> StateT (U.Universe ImageWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

adjustSubjectEnergy
  :: Double -> Simple Lens Summary Double -> Simple Lens Summary Double
    -> String -> StateT Experiment IO ()
adjustSubjectEnergy deltaE adultSelector childSelector reason = do
  x <- use subject
  (x', adultDeltaE, childDeltaE)
     <- withUniverse $ adjustEnergy reason deltaE x
  (summary . adultSelector) += adultDeltaE
  when (childDeltaE /= 0) $ (summary . childSelector) += childDeltaE
  assign subject x'

adjustObjectEnergy
  :: Simple Lens Experiment Object -> Double
    -> Simple Lens Summary Double -> Simple Lens Summary Double -> String
      -> StateT Experiment IO ()
adjustObjectEnergy
    objectSelector deltaE adultSelector childSelector reason = do
  x <- use objectSelector
  case x of
    AObject a -> do
      (a', adultDeltaE, childDeltaE)
        <- withUniverse $ adjustEnergy reason deltaE a
      (summary . adultSelector) += adultDeltaE
      when (childDeltaE /= 0) $ (summary . childSelector) += childDeltaE
      assign objectSelector (AObject a')
    IObject _ _ -> return ()

adjustSubjectPassion
  :: StateT Experiment IO ()
adjustSubjectPassion = do
  x <- use subject
  assign subject (adjustPassion x)

letSubjectReflect
  :: Response Action -> StateT Experiment IO ()
letSubjectReflect r = do
  x <- use subject
  p1 <- objectAppearance <$> use directObject
  p2 <- objectAppearance <$> use indirectObject
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
