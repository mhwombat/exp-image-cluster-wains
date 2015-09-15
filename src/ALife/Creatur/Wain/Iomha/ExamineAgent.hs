------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Do a full analysis of a wain and generate a report.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.Wain.Iomha.Experiment
import ALife.Creatur.Wain.ExamineAgent (fetchWains, examine)
import System.Environment

main :: IO ()
main = do
  (f:_) <- getArgs
  ws <- fetchWains f :: IO [ImageWain]
  mapM_ examine ws
