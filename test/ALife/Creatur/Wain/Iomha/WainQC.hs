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

-- strawMan :: Gen ImageWain
-- strawMan = Wain <$> pure ""       -- name
--                 <*> arbitrary     -- appearance
--                 <*> arbitrary     -- brain
--                 <*> arbitrary     -- devotion
--                 <*> arbitrary     -- age of maturity
--                 <*> arbitrary     -- delta passion
--                 <*> arbitrary     -- energy
--                 <*> arbitrary     -- passion
--                 <*> arbitrary     -- age
--                 <*> pure []       -- litter
--                 <*> arbitrary     -- children borne during lifetime
--                 <*> arbitrary     -- children weanded during lifetime
--                 <*> arbitrary     -- # of wins
--                 <*> pure ([],[])  -- genome
--                 <*> arbitrary     -- size

-- -- | Can't just generate an arbitrary genome and build an agent from
-- --   it, because random genomes tend to be invalid.
-- arbWain :: Gen ImageWain
-- arbWain = do
--   n <- arbitrary
--   a1 <- strawMan
--   a2 <- strawMan
--   let g1 = write a1
--   let g2 = write a2
--   let r = runDiploidReader (buildWainFromGenome False n) (g1, g2)
--   case r of
--     (Left s)   -> error . show $ s
--     (Right r') -> return r'

-- sizedArbWain :: Int -> Gen ImageWain
-- sizedArbWain n = do
--   w <- arbWain
--   if n < 1
--     then do
--       cs <- listOf arbWain
--       return $ set litter cs w
--     else return w

-- instance Arbitrary ImageWain where
--   arbitrary = sized sizedArbWain

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

-- prop_novelty_btw_0_and_1 :: Image -> Image -> ImageWain -> Property
-- prop_novelty_btw_0_and_1 p1 p2 w = property . and . map inRange $ ns
--     where (_, _, _, ns) = chooseAction [p1, p2] w
--           inRange x = 0 <= x && x <= 1

test :: Test
test = testGroup "ALife.Creatur.Wain.Iomha.WainQC"
  [
    testProperty "prop_idealPopControlDeltaE_counteracts_pop_growth"
      prop_idealPopControlDeltaE_counteracts_pop_growth,
    testProperty "prop_idealPopControlDeltaE_counteracts_learning"
      prop_idealPopControlDeltaE_counteracts_learning
  ]
