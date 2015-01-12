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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ALife.Creatur.Wain.Iomha.FMRI
  (
    drawClassifier,
    writeFmri
  ) where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.Brain (classifier)
import ALife.Creatur.Wain.GeneticSOM (toList)
import ALife.Creatur.Wain.Iomha.Image
import Data.Colour.SRGB
import Data.List.Split
import Data.Word
import Diagrams.Prelude
import Diagrams.TwoD.Text
import Diagrams.Backend.Cairo

grey2colour :: Word8 -> Colour Double
grey2colour x = sRGB x' x' x'
  where x' = fromIntegral x / 255

-- grey2colour :: Word8 -> AlphaColour Double
-- grey2colour n = opaque $ sRGB24 (255 - n) (255 - n) (255 - n) 

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

-- image2raster :: Image -> DImage Embedded
-- image2raster img = raster f (iWidth img) (iHeight img)
--   where f r c = grey2colour (pixelAt img r c)
-- -- raster :: (Int -> Int -> AlphaColour Double) -> Int -> Int -> DImage Embedded

-- cream :: (Ord b, Floating b) => Colour b
-- cream = sRGB24 255 255 224

drawNode
  :: (Renderable Text b, Renderable (Path R2) b)
    => (Label, Image) -> Diagram b R2
drawNode (index, img) = label `atop` pic `atop` area
  where area = rect 1 1.1 # lw none
        label = translateY 0.475 $ text (show index) # fc black # fontSize (Local 0.08)
        imgSizeSpec = mkSizeSpec (Just 0.95) (Just 0.95)
        pic = translateY 0.4 . centerX . sized imgSizeSpec $ image2diagram img

drawRow
  :: (Renderable Text b, Renderable (Path R2) b)
     => [(Label, Image)] -> Diagram b R2
drawRow = hcat . map drawNode

drawClassifier
  :: (Renderable Text b, Renderable (Path R2) b)
     => [(Label, Image)] -> Diagram b R2
drawClassifier = mconcat . zipWith translateY [0,-1.2..] . map (alignL . drawRow) . chunksOf 6

writeFmri :: Wain Image a -> FilePath -> IO ()
writeFmri w f = renderCairo f ss diagram
  where ss = mkSizeSpec (Just 500) Nothing
        diagram = drawClassifier . toList . classifier . brain $ w :: Diagram B R2
