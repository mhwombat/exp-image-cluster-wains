------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.WainQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Iomha.WainQC
  (
    test
  ) where

import ALife.Creatur.Wain.Iomha.Wain
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

prop_idealPopControlDeltaE_counteracts_pop_growth
  :: Positive Int -> Positive Int -> Positive Int -> Double -> Property
prop_idealPopControlDeltaE_counteracts_pop_growth idealPop p deltaP e
  = not (isNaN ec1 || isNaN ec2 || isNegativeZero ec1) ==> ec2 < ec1
  where ec1 = idealPopControlDeltaE pIdeal p1 e
        ec2 = idealPopControlDeltaE pIdeal p2 e
        pIdeal = getPositive idealPop
        p1 = getPositive p
        p2 = p1 + getPositive deltaP

prop_idealPopControlDeltaE_counteracts_learning
  :: Positive Int -> Positive Int -> Double -> Positive Double -> Property
prop_idealPopControlDeltaE_counteracts_learning idealPop pop e deltaE
  = not (isNaN ec1 || isNaN ec2) ==> ec2 < ec1
  where ec1 = idealPopControlDeltaE pIdeal p e
        ec2 = idealPopControlDeltaE pIdeal p e2
        pIdeal = getPositive idealPop
        p = getPositive pop
        e2 = e + getPositive deltaE

test :: Test
test = testGroup "ALife.Creatur.Wain.Iomha.WainQC"
  [
    testProperty "prop_idealPopControlDeltaE_counteracts_pop_growth"
      prop_idealPopControlDeltaE_counteracts_pop_growth,
    testProperty "prop_idealPopControlDeltaE_counteracts_learning"
      prop_idealPopControlDeltaE_counteracts_learning
  ]
