------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.ExperimentQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Iomha.ExperimentQC
  (
    test
  ) where

import ALife.Creatur.Wain.Iomha.Experiment
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

prop_idealPopControlDeltaE_counteracts_overpopulation
  :: Positive Int -> Positive Int -> Positive Int -> Positive Double -> Property
prop_idealPopControlDeltaE_counteracts_overpopulation
  (Positive pIdeal) (Positive deltaP) (Positive deltaP2) (Positive deltaE)
    = property $ ec1 < 0 && ec2 < 0 && ec2 < ec1
  where p1 = pIdeal + deltaP
        p2 = p1 + deltaP2
        ec1 = idealPopControlDeltaE pIdeal p1 deltaE
        ec2 = idealPopControlDeltaE pIdeal p2 deltaE

prop_idealPopControlDeltaE_counteracts_underpopulation
  :: Positive Int -> Positive Int -> Positive Int -> Positive Double -> Property
prop_idealPopControlDeltaE_counteracts_underpopulation
  (Positive p1) (Positive deltaP) (Positive deltaP2) (Positive deltaE)
    = property $ ec1 > 0 && ec2 > 0 && ec2 < ec1
  where p2 = p1 + deltaP
        pIdeal = p2 + deltaP2
        ec1 = idealPopControlDeltaE pIdeal p1 deltaE
        ec2 = idealPopControlDeltaE pIdeal p2 deltaE

test :: Test
test = testGroup "ALife.Creatur.Wain.Iomha.ExperimentQC"
  [
    testProperty "prop_idealPopControlDeltaE_counteracts_overpopulation"
      prop_idealPopControlDeltaE_counteracts_overpopulation,
    testProperty "prop_idealPopControlDeltaE_counteracts_underpopulation"
      prop_idealPopControlDeltaE_counteracts_underpopulation
  ]
