------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.FMRI
-- Copyright   :  (c) Amy de Buitléir 2014-2015
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
import ALife.Creatur.Wain.Iomha.ImageThinker
import Control.Lens hiding ((#), none)
import Data.Colour.SRGB
import Data.List.Split
import Data.Typeable
import Data.Word
import Diagrams.Prelude hiding (view)
import Diagrams.TwoD.Text
-- import Diagrams.Backend.Cairo
import Diagrams.Backend.SVG

grey2colour :: Word8 -> Colour Double
grey2colour x = sRGB x' x' x'
  where x' = fromIntegral x / 255

colour2square
  :: (Typeable (N b), HasStyle b, TrailLike b, V b ~ V2)
    => Colour Double -> b
colour2square c = square 0.1 # fc c # lw none

imageRow
  :: (Typeable (N c), HasOrigin c, Juxtaposable c, HasStyle c,
    TrailLike c, Semigroup c, Monoid c, V c ~ V2)
      => [Word8] -> c
imageRow = hcat . map (colour2square . grey2colour)

image2diagram
  :: (Typeable (N c), HasOrigin c, Juxtaposable c, HasStyle c, TrailLike c,
    Semigroup c, Monoid c, V c ~ V2)
     => Image -> c
image2diagram = vcat . map imageRow . pixelArray

drawNode
  :: (Typeable n, Floating n, Fractional n, RealFloat n,
    Renderable (Text n) b, Renderable (Path V2 n) b)
      => (Label, Image) -> QDiagram b V2 n Any
drawNode (i, img) = label `atop` pic `atop` area
  where area = rect 1 1.1 # lw none
        label = translateY 0.475 $ text (show i) # fc black # fontSize (local 0.08)
        imgSizeSpec = mkSizeSpec2D (Just 0.95) (Just 0.95)
        pic = translateY 0.4 . centerX . sized imgSizeSpec $ image2diagram img

drawRow
  :: (RealFloat n, Typeable n, Renderable (Text n) b,
    Renderable (Path V2 n) b)
      => [(Label, Image)] -> QDiagram b V2 n Any
drawRow = hcat . map drawNode

drawClassifier
  :: (Enum n, RealFloat n, Typeable n, Renderable (Path V2 n) b,
    Renderable (Text n) b)
      => [(Label, Image)] -> QDiagram b V2 n Any
drawClassifier = mconcat . zipWith translateY [0,-1.2..] . map (alignL . drawRow) . chunksOf 6

writeFmri :: Wain Image ImageThinker a -> FilePath -> IO ()
writeFmri w f = renderSVG f ss diagram
  where ss = mkSizeSpec2D (Just 500) Nothing
        c = view classifier . view brain $ w
        diagram = drawClassifier . toList $ c :: QDiagram SVG V2 Double Any
