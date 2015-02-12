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

prop_idealCoopDeltaE_counteracts_pop_growth
  :: Positive Double -> Positive Int -> Positive Int -> Positive Int
    -> Double -> Property
prop_idealCoopDeltaE_counteracts_pop_growth coopRate idealPop p deltaP e
  = not (isNaN ec1 || isNaN ec2 || isNegativeZero ec1) ==> ec2 < ec1
  where ec1 = idealCoopDeltaE r pIdeal p1 e
        ec2 = idealCoopDeltaE r pIdeal p2 e
        r = getPositive coopRate
        pIdeal = getPositive idealPop
        p1 = getPositive p
        p2 = p1 + getPositive deltaP

prop_idealCoopDeltaE_counteracts_learning
  :: Positive Double -> Positive Int -> Positive Int -> Double
    -> Positive Double -> Property
prop_idealCoopDeltaE_counteracts_learning coopRate idealPop pop e deltaE
  = not (isNaN ec1 || isNaN ec2) ==> ec2 < ec1
  where ec1 = idealCoopDeltaE r pIdeal p e
        ec2 = idealCoopDeltaE r pIdeal p e2
        r = getPositive coopRate
        pIdeal = getPositive idealPop
        p = getPositive pop
        e2 = e + getPositive deltaE

-- prop_idealCoopDeltaE_counteracts_coop_rate_growth
--   :: Positive Double -> Positive Double -> Positive Int -> Positive Int
--     -> Double -> Property
-- prop_idealCoopDeltaE_counteracts_coop_rate_growth coopRate coopDelta idealPop pop e
--   = not (isNaN ec1 || isNaN ec2 || isInfinite ec1 || isNegativeZero ec1)
--     && deltaR > 1e-100 ==> ec2 < ec1
--   where ec1 = idealCoopDeltaE r1 pIdeal p e
--         ec2 = idealCoopDeltaE r2 pIdeal p e
--         deltaR = getPositive coopDelta
--         r1 = getPositive coopRate
--         r2 = r1 + deltaR
--         pIdeal = getPositive idealPop
--         p = getPositive pop

test :: Test
test = testGroup "ALife.Creatur.Wain.Iomha.WainQC"
  [
    testProperty "prop_idealCoopDeltaE_counteracts_pop_growth"
      prop_idealCoopDeltaE_counteracts_pop_growth,
    testProperty "prop_idealCoopDeltaE_counteracts_learning"
      prop_idealCoopDeltaE_counteracts_learning
    -- testProperty "prop_idealCoopDeltaE_counteracts_coop_rate_growth"
    --   prop_idealCoopDeltaE_counteracts_coop_rate_growth
  ]
