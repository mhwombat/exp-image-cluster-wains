------------------------------------------------------------------------
-- |
-- Module      :  SOMTest
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
--
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.Wain.Classifier
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Iomha.FMRI
import ALife.Creatur.Wain.Iomha.Image
import ALife.Creatur.Wain.Iomha.ImageTweaker
import Control.Monad
import Control.Monad.Random
import Data.List
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (fc, view)
import System.Directory

makeClassifier :: RandomGen r => Rand r (GeneticSOM Image ImageTweaker)
makeClassifier = do
  imgs <- replicateM 15 (randomImageR 28 28 (0, 1))
  -- let imgs = replicate classifierSize $ blankImage w h
  let fc = ExponentialParams 1 0.05
  return $ buildClassifier fc ImageTweaker imgs

update :: GeneticSOM Image ImageTweaker -> Image -> GeneticSOM Image ImageTweaker
update som img = som'
  where (_, _, _, som') = reportAndTrain som img

dir :: String
dir = "/home/eamybut/mnist/testData/"

main :: IO ()
main = do
  files <- map (dir ++) . take 100 . drop 2 <$> getDirectoryContents dir
  imgs <- mapM readImage files
  som <- evalRandIO makeClassifier
  let som' = foldl' update som imgs
  let ss = mkSizeSpec2D (Just 500) Nothing
  let diagram = drawClassifier . toList $ som' :: QDiagram SVG V2 Double Any
  renderSVG "SOMTest.svg" ss diagram
