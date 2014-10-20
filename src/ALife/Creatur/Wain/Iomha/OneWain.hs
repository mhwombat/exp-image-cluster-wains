------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.OneWain
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Main where

import ALife.Creatur.Wain
import qualified ALife.Creatur.Wain.Brain as B
import ALife.Creatur.Wain.Condition
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Iomha.Action
import ALife.Creatur.Wain.Iomha.FMRI
import ALife.Creatur.Wain.Iomha.Image
import ALife.Creatur.Wain.Iomha.ImageDB
import ALife.Creatur.Wain.Iomha.Universe
import ALife.Creatur.Wain.Iomha.Wain
import qualified ALife.Creatur.Wain.Response as R
import qualified ALife.Creatur.Wain.Scenario as S
import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import qualified Data.Datamining.Clustering.SSOM as SOM
import Diagrams.Backend.Cairo
import Diagrams.Prelude

testWain :: Int -> Int -> [Image] -> ImageWain
testWain w h ps = buildWainAndGenerateGenome "Fred" app b 0 m p
  where c = buildGeneticSOM cf ps
        cf = SOM.Exponential 1 1
        d = buildGeneticSOM df [] -- not used
        df = SOM.Exponential 1 1
        b = B.buildBrain c d
        m = 1
        p = 0.001
        app = stripedImage w h

initialResponse :: Int -> R.Response Action
initialResponse n = R.Response s Ignore o
  where s = S.Scenario p p c
        p = replicate n 0
        c = Condition 0.5 0.5 0
        o = Just 1

initialResponses :: Int -> [R.Response Action]
initialResponses n = replicate 7 (initialResponse n)

learnOne :: ImageWain -> (Image, String) -> IO ImageWain
learnOne agent (img, imgName) = do
  putStrLn $ "Examining " ++ imgName
  let (_, _, _, _, agent') = classify img agent
  return agent'

learnBatch :: ImageWain -> ImageDB -> IO ImageWain
learnBatch agent db = do
  imageData <- evalStateT (replicateM 1000 anyImage) db
  foldM learnOne agent imageData

main :: IO ()
main = do
  u <- loadUniverse
  let (w, h) = (uImageWidth u, uImageHeight u)
  ps <- evalRandIO (replicateM 15 $ randomImage w h)
  let w0 = testWain w h ps
  a <- learnBatch w0 (uImageDB u)
  let ss = mkSizeSpec (Just 500) Nothing
  let diagram = drawClassifier . toList . B.classifier . brain $ a :: Diagram B R2
  renderCairo "oneWain.svg" ss diagram
 
