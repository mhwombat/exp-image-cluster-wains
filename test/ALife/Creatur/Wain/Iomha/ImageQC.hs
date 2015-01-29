------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.ImageQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Iomha.ImageQC
  (
    test
  ) where

import ALife.Creatur.Wain.Iomha.Image
import ALife.Creatur.Wain.Iomha.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
-- import Data.Array.Repa (Array, DIM3, DIM2, Shape, Source,
--   toList, listOfShape, extent, shapeOfList, size)
-- import Data.Array.Repa.Eval (fromList)
-- import Data.Array.Repa.Repr.ForeignPtr (F)
-- import Data.Word (Word8)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- arbShape :: Shape sh => Int -> Gen sh
-- arbShape nDims = do
--   dims <- vectorOf nDims (choose (0,10)) -- keep the images small for testing
--   return $ shapeOfList dims

-- arbPixels :: Shape sh => sh -> Gen [Word8]
-- arbPixels s = vectorOf (size s) arbitrary

-- arbArray :: Shape sh => Int -> Gen (Array F sh Word8)
-- arbArray nDims = do
--   s <- arbShape nDims
--   ps <- arbPixels s
--   return $ fromList s ps

sizedArbImage :: Int -> Gen Image
sizedArbImage n = do
  w <- choose (0,n)
  let h = n - w
  ps <- vectorOf (w*h) arbitrary
  return $ Image w h ps

instance Arbitrary Image where
  arbitrary = sized sizedArbImage

-- instance Arbitrary Image where
--   arbitrary = oneof [RGBA <$> arbArray 3, RGB <$> arbArray 3,
--                      BGRA <$> arbArray 3, BGR <$> arbArray 3,
--                      Grey <$> arbArray 2]

-- showArray :: Shape sh => Array F sh Word8 -> String
-- showArray arr = sh ++ " " ++ xs
--   where sh = show . listOfShape . extent $ arr
--         elems = toList arr :: [Word8]
--         xs = show elems

-- instance Show (Array F DIM3 Word8) where
--   show = showArray
  
-- instance Show (Array F DIM2 Word8) where
--   show = showArray

-- data ValidImage = ValidImage Image deriving (Eq, Show)

-- instance Arbitrary ValidImage where
--   arbitrary = ValidImage <$> dimensionedArbImage imageWidth imageHeight

-- prop_imageToArray_round_trippable :: Image -> Property
-- prop_imageToArray_round_trippable x = property $ x' == x
--   where x' = arrayToImage . imageToArray $ x

test :: Test
test = testGroup "ALife.Creatur.Wain.Iomha.ImageQC"
  [
    -- testProperty "prop_imageToArray_round_trippable"
    --   prop_imageToArray_round_trippable,
    testProperty "prop_serialize_round_trippable - Image"
      (prop_serialize_round_trippable :: Image -> Property),
    testProperty "prop_genetic_round_trippable - Image"
      (prop_genetic_round_trippable (==) :: Image -> Property),
    testProperty "prop_diploid_identity - Image"
      (prop_diploid_identity (==) :: Image -> Property)
    -- testProperty "prop_generator_never_fails - Image"
    --   (prop_generator_never_fails :: ValidImage -> Property)
  ]
