------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.FMRIMain
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
import ALife.Creatur.Wain.Brain
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Iomha.FMRI
import ALife.Creatur.Wain.Iomha.Universe
import ALife.Creatur.Wain.Iomha.Wain
import Control.Monad.State
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import System.Environment

getWain :: String -> StateT (Universe ImageWain) IO ImageWain
getWain s = do
  a <- getAgent s
  case a of
    (Right agent) -> return agent
    (Left msg)    -> error msg 
  
getWainName :: IO String
getWainName = do
  args <- getArgs
  if null args
    then error "Need to supply a wain name!"
    else return (head args)

main :: IO ()
main = do
  u <- loadUniverse
  n <- getWainName
  w <- evalStateT (getWain n) u
  let ss = mkSizeSpec (Just 500) Nothing
  let diagram = drawClassifier . toList . classifier . brain $ w :: Diagram B R2
  let outputFileName = n ++ ".svg"
  renderCairo outputFileName ss diagram
