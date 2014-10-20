------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.FMRI
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
module ALife.Creatur.Wain.Iomha.FMRI
  (
    drawClassifier
  ) where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.Iomha.Image
import Data.Colour.SRGB
import Data.List.Split
import Data.Word
import Diagrams.Prelude
import Diagrams.TwoD.Text

grey2colour :: Word8 -> Colour Double
grey2colour x = sRGB x' x' x'
  where x' = (fromIntegral x)/255

colour2square
  :: (HasStyle b, Transformable b, TrailLike b, V b ~ R2)
    => Colour Double -> b
colour2square c = square 0.1 # fc c # lw none

imageRow
  :: (HasOrigin c, Juxtaposable c, HasStyle c, Transformable c,
    TrailLike c, Semigroup c, Monoid c, V c ~ R2)
      => [Word8] -> c
imageRow = hcat . map (colour2square . grey2colour)

image2diagram
  :: (HasOrigin c, Juxtaposable c, HasStyle c, TrailLike c,
    Transformable c, Semigroup c, Monoid c, V c ~ R2)
     => Image -> c
image2diagram = vcat . map imageRow . pixelArray

-- grey2colour :: Word8 -> AlphaColour Double
-- grey2colour n = opaque $ sRGB24 (255 - n) (255 - n) (255 - n)
 
-- image2raster :: Image -> DImage Embedded
-- image2raster img = raster f (iWidth img) (iHeight img)
--   where f r c = grey2colour (pixelAt img r c)
-- -- raster :: (Int -> Int -> AlphaColour Double) -> Int -> Int -> DImage Embedded

-- cream :: (Ord b, Floating b) => Colour b
-- cream = sRGB24 255 255 224

drawNode
  :: (Renderable Text b, Renderable (Path R2) b)
    => (Label, Image) -> Diagram b R2
drawNode (index, img) = label `atop` pic
  where label = translateY (1.3) $ text (show index) # fc black # fontSize (Local 0.4)
        imgSizeSpec = mkSizeSpec (Just 2) (Just 2)
        pic = translateY 1 . translateX (-1) . sized imgSizeSpec $ image2diagram img

drawRow
  :: (Renderable Text b, Renderable (Path R2) b)
     => [(Label, Image)] -> Diagram b R2
drawRow = hcat . map drawNode

drawClassifier
  :: (Renderable Text b, Renderable (Path R2) b)
     => [(Label, Image)] -> Diagram b R2
drawClassifier = mconcat . zipWith translateY [0,-3..] . map (centerX . drawRow) . chunksOf 8
