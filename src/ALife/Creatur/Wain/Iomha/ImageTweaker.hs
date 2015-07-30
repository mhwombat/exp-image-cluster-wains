------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.ImageTweaker
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for comparing and adjusting images.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Iomha.ImageTweaker
  (
    ImageTweaker(..)
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Tweaker(..))
import ALife.Creatur.Wain.Iomha.Image (Image, imageDiff,
  makeImageSimilar)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data ImageTweaker = ImageTweaker deriving (Eq, Show, Generic)

instance Tweaker ImageTweaker where
  type Pattern ImageTweaker = Image
  diff _ = imageDiff
  adjust _ = makeImageSimilar

instance Serialize ImageTweaker
instance W8.Genetic ImageTweaker
instance Diploid ImageTweaker
