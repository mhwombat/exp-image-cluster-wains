------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.ImageThinker
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
module ALife.Creatur.Wain.Iomha.ImageThinker
  (
    ImageThinker(..)
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Thinker(..))
import ALife.Creatur.Wain.Iomha.Image (Image, imageDiff,
  makeImageSimilar)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data ImageThinker = ImageThinker deriving (Eq, Show, Generic)

instance Thinker ImageThinker where
  type Pattern ImageThinker = Image
  diff _ = imageDiff
  adjust _ = makeImageSimilar

instance Serialize ImageThinker
instance W8.Genetic ImageThinker
instance Diploid ImageThinker
