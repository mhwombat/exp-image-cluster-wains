------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module Main where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.Brain
import ALife.Creatur.Wain.Response
import qualified ALife.Creatur.Wain.Scenario as Scenario
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Iomha.Action
import ALife.Creatur.Wain.Iomha.Wain
import ALife.Creatur.Wain.Iomha.Universe
import ALife.Creatur.Wain.PlusMinusOne
import ALife.Creatur.Wain.UnitInterval
import Control.Lens
import Control.Monad.State
import Data.Map.Strict (elems)
import System.Environment
import Text.Printf (printf)

getAndExamineAll :: StateT (Universe ImageWain) IO ()
getAndExamineAll = do
  names <- agentIds
  mapM_ getAndExamine names
  
getAndExamine :: String -> StateT (Universe ImageWain) IO ()
getAndExamine s = do
  a <- getAgent s
  case a of
    (Right agent) -> liftIO $ examine agent
    (Left msg)    -> liftIO $ putStrLn msg 
  
examine :: ImageWain -> IO ()
examine a = do
  putStrLn $ "name: " ++ show (view name a)
  -- appearance
  -- brain
  putStrLn $ "devotion: " ++ printf "%5.3f" (uiToDouble $ view devotion a)
  putStrLn $ "ageOfMaturity: " ++ show (view ageOfMaturity a)
  putStrLn $ "passionDelta: " ++ show (view passionDelta a)
  putStrLn $ "energy: " ++ printf "%5.3f" (uiToDouble $ view energy a)
  putStrLn $ "passion: " ++ printf "%5.3f" (uiToDouble $ view passion a)
  putStrLn $ "age: " ++ show (view age a)
  putStrLn $ "total # children borne: "
    ++ show (view childrenBorneLifetime a)
  putStrLn $ "total # children weaned: "
    ++ show (view childrenWeanedLifetime a)
  putStrLn $ "litter size: " ++ show (length . view litter $ a)
  putStrLn $ "counts=" ++ show (elems . view counterMap . view classifier . view brain $ a)
  putStrLn $ "swagger: " ++ show (view swagger a)
  putStrLn $ "size: " ++ show (view wainSize a)
  putStrLn $ "SQ: " ++ show (schemaQuality . view decider . view brain $ a)
  putStrLn $ "Number of classifier models: " ++ show (numModels . view classifier . view brain $ a)
  putStrLn $ "Classifier learning function " ++ show (view exponentialParams . view classifier . view brain $ a)
  putStrLn $ "Number of decider models: " ++ show (numModels . view decider . view brain $ a)
  putStrLn $ "Decider learning function " ++ show (view exponentialParams . view decider . view brain $ a)
  -- putStrLn "------------------------"
  -- putStrLn "Mental models of vectors"
  -- putStrLn "------------------------"
  -- mapM_ putStrLn $ concatMap (prettyAudioPattern 9) (toList . classifier . brain $ a)
  putStrLn "-----------------"
  putStrLn "Response models"
  putStrLn "-----------------"
  mapM_ putStrLn $ concatMap prettyResponseModel (toList . view decider . view brain $ a)
  -- putStrLn "--------"
  -- putStrLn "Raw data"
  -- putStrLn "--------"
  -- putStrLn $ show a

prettyResponseModel :: (Label, Response Action) -> [String]
prettyResponseModel (l, r) =
  [ "Model " ++ show l,
    "Differences: "
      ++ formatVector "%5.3f" (map uiToDouble . head . view (scenario . Scenario.diffs) $ r),
    "Energy: " ++ show (head . view (scenario . Scenario.condition) $ r),
    "Passion: " ++ show ((!!2) . view (scenario . Scenario.condition) $ r),
    "Action: " ++ show (view action r),
    "Expected happiness change: "
      ++ maybe "" (printf "%.3g" . pm1ToDouble) (view outcome r),
    "-----" ]

formatVector :: String -> [Double] -> String
formatVector fmt = unwords . map (printf fmt)

main :: IO ()
main = do
  u <- loadUniverse
  t <- evalStateT currentTime u
  putStrLn $ "Universe time is " ++ show t

  args <- getArgs
  if null args
    then
      evalStateT getAndExamineAll u
    else do
      let s = head args
      evalStateT (getAndExamine s) u
