------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2013-2014
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
import ALife.Creatur.Wain.Condition
import ALife.Creatur.Wain.Response
import qualified ALife.Creatur.Wain.Scenario as Scenario
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Iomha.Action
import ALife.Creatur.Wain.Iomha.Wain
import ALife.Creatur.Wain.Iomha.Universe
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
  putStrLn $ "name: " ++ show (name a)
  -- appearance
  -- brain
  putStrLn $ "devotion: " ++ printf "%5.3f" (devotion a)
  putStrLn $ "ageOfMaturity: " ++ show (ageOfMaturity a)
  putStrLn $ "passionDelta: " ++ show (passionDelta a)
  putStrLn $ "energy: " ++ printf "%5.3f" (energy a)
  putStrLn $ "passion: " ++ printf "%5.3f" (passion a)
  putStrLn $ "age: " ++ show (age a)
  putStrLn $ "total # children borne: "
    ++ show (childrenBorneLifetime a)
  putStrLn $ "total # children weaned: "
    ++ show (childrenWeanedLifetime a)
  putStrLn $ "litter size: " ++ show (length $ litter a)
  putStrLn $ "counts=" ++ show (elems . counterMap . classifier $ brain a)
  putStrLn $ "swagger: " ++ show (swagger a)
  putStrLn $ "size: " ++ show (wainSize a)
  putStrLn $ "SQ: " ++ show (schemaQuality . decider . brain $ a)
  putStrLn $ "Number of classifier models: " ++ show (numModels . classifier . brain $ a)
  putStrLn $ "Classifier learning function " ++ show (learningFunction . classifier . brain $ a)
  putStrLn $ "Number of decider models: " ++ show (numModels . decider . brain $ a)
  putStrLn $ "Decider learning function " ++ show (learningFunction . decider . brain $ a)
  -- putStrLn "------------------------"
  -- putStrLn "Mental models of vectors"
  -- putStrLn "------------------------"
  -- mapM_ putStrLn $ concatMap (prettyAudioPattern 9) (toList . classifier . brain $ a)
  putStrLn "-----------------"
  putStrLn "Response models"
  putStrLn "-----------------"
  mapM_ putStrLn $ concatMap prettyResponseModel (toList . decider . brain $ a)
  -- putStrLn "--------"
  -- putStrLn "Raw data"
  -- putStrLn "--------"
  -- putStrLn $ show a

prettyResponseModel :: (Label, Response Action) -> [String]
prettyResponseModel (l, r) =
  [ "Model " ++ show l,
    "Differences: "
      ++ formatVector "%5.3f" (Scenario.directObject . scenario $ r),
    "Energy: " ++ show (cEnergy . Scenario.condition . scenario $ r),
    "Passion: " ++ show (cPassion . Scenario.condition . scenario $ r),
    "Action: " ++ show (action r),
    "Expected happiness change: "
      ++ maybe "" (printf "%.3g") (outcome r),
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
