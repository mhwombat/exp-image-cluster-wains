------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.TestUtils
-- Copyright   :  (c) Amy de Buitléir 2013-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck test utilities.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Iomha.TestUtils
  (
    arb8BitDouble,
    arb8BitInt,
    prop_serialize_round_trippable,
    prop_genetic_round_trippable,
    prop_diploid_identity,
    prop_show_read_round_trippable
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Util (fromEither)
import ALife.Creatur.Wain.Util (scaleFromWord8, scaleWord8ToInt)
import Data.Serialize (Serialize, encode, decode)
import Data.Word (Word8)
import Test.QuickCheck

arb8BitDouble :: (Double, Double) -> Gen Double
arb8BitDouble interval = do 
  x <- arbitrary :: Gen Word8
  return $ scaleFromWord8 interval x
  
arb8BitInt :: (Int, Int) -> Gen Int
arb8BitInt interval = do 
  x <- arbitrary :: Gen Word8
  return $ scaleWord8ToInt interval x
  
prop_serialize_round_trippable :: (Eq a, Serialize a) => a -> Property
prop_serialize_round_trippable x = property $ x' == Right x
  where bs = encode x
        x' = decode bs

prop_genetic_round_trippable :: (Eq g, G.Genetic g, Show g) =>
  (g -> g -> Bool) -> g -> Property
prop_genetic_round_trippable equiv g = property $ g' `equiv` g
  where x = G.write g
        g' = fromEither (error "read returned Nothing") . G.read $ x

prop_diploid_identity :: Diploid g => (g -> g -> Bool) -> g -> Property
prop_diploid_identity equiv g = property $ express g g `equiv` g

prop_show_read_round_trippable :: (Read a, Show a) => (a -> a -> Bool) -> a -> Property
prop_show_read_round_trippable equiv x
  = property $ (read . show $ x) `equiv` x
    
