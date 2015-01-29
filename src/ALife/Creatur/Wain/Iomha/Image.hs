------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.Image
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Image utilities.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Iomha.Image
  (
    Image(..),
    mkImage,
    pixelCount,
    pixelAt,
    pixelArray,
    blankImage,
    stripedImage,
    randomImage,
    readImage,
    writeImage,
    -- imageToArray,
    -- arrayToImage
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic, put, get,
  Reader, putRawWord8s, getRawWord8s)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.Util (forceIntToWord8, word8ToInt)
import qualified Codec.Picture as P
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Random (Rand, RandomGen, getRandoms)
-- import Data.Array.Repa (extent, toList)
-- import Data.Array.Repa.Eval (fromList)
-- import Data.Array.Repa.Index ((:.)(..), Z(..))
-- import qualified Data.Array.Repa.IO.DevIL as I
-- import Data.Array.Repa.Shape (listOfShape)
import Data.Datamining.Pattern (Pattern(..))
import Data.List.Split (chunksOf)
import Data.Serialize (Serialize)
import Data.Vector.Storable (Vector, toList, fromList)
import Data.Word (Word8)
import GHC.Generics (Generic)

data Image = Image { iWidth :: Int, iHeight :: Int, pixels :: [Word8] }
  deriving (Eq, Generic, Show)

pixelCount :: Image -> Int
pixelCount img = iWidth img * iHeight img

mkImage :: Int -> Int -> [Word8] -> Image
mkImage w h ps = Image w h ps'
  where ps' = take (w*h) (ps ++ repeat 0)

instance Serialize Image

instance Genetic Image where
  put img = do
    put . forceIntToWord8 . iWidth $ img
    put . forceIntToWord8 . iHeight $ img
    putRawWord8s . pixels $ img
  get = do
    w <- get
    h <- get
    getPixels w h

getPixels
  :: Either [String] Word8
     -> Either [String] Word8
     -> Reader (Either [String] Image)
getPixels (Right w) (Right h) = do
  let w' = word8ToInt w
  let h' = word8ToInt h
  ps <- getRawWord8s (w' * h')
  return $ mkImage <$> pure w' <*> pure h' <*> ps
getPixels (Left s) (Right _) = return $ Left s
getPixels (Right _) (Left s) = return $ Left s
getPixels (Left s) (Left t) = return $ Left (s ++ t)

pixelAt :: Image -> Int -> Int -> Word8
pixelAt (Image w _ ps) r c = ps !! (r*w + c)

pixelArray :: Image -> [[Word8]]
pixelArray (Image w _ ps) = chunksOf w ps

-- forceEitherWord8List :: Either [String] [Word8] -> [Word8]
-- forceEitherWord8List x =
--   case x of
--     Left _ -> []
--     Right ws -> ws

-- instance Show Image where
--   show (Image w h _) = "<" ++ show w ++ "x" ++ show h ++ " pixels>"

instance Pretty Image where
  pretty (Image w h xs) = show w ++ "x" ++ show h ++ concatMap f xs
    where f x = ' ' : pretty x

instance Pattern Image where
  type Metric Image = Double
  difference a b = avgDelta / 255
    where xs = map fromIntegral . take l $ pixels a ++ repeat 0 :: [Double]
          ys = map fromIntegral . take l $ pixels b ++ repeat 0 :: [Double]
          l = max (pixelCount a) (pixelCount b)
          diff = sum . map abs $ zipWith (-) xs ys
          avgDelta = diff / fromIntegral l
  makeSimilar target amount a
    = a { pixels=adjust (pixels target) amount (pixels a) }

instance Diploid Image where
  express a b = mkImage w h ps
    where w = min (iWidth a) (iWidth b)
          h = min (iHeight a) (iHeight b)
          ps = zipWith min (pixels a) (pixels b)

-- Used for generating the initial brain models in the initial
-- population.
randomImage :: RandomGen r => Int -> Int -> Rand r Image
randomImage w h = fmap (mkImage w h . take (w*h)) getRandoms

-- Used for generating the initial brain models in the initial
-- population.
blankImage :: Int -> Int -> Image
blankImage w h = Image w h $ replicate (w*h) 0

indices :: Int -> Int -> [(Int, Int)]
indices w h = [(i,j) | i <- [0..h-1], j <- [0..w-1]]
  -- row, column, colour channel
  
-- indices :: Int -> Int -> [(Int, Int, Int)]
-- indices w h = [(i,j) | i <- [0..h-1], j <- [0..w-1]]
--   -- row, column, colour channel
  
stripedImage :: Int -> Int -> Image
stripedImage w h = Image w h . map f $ indices w h
  where f (i,_) | i < halfWidth = 255
                | otherwise     = 0
        halfWidth = round $ (fromIntegral w :: Double) / 2

readImage :: FilePath -> IO Image
readImage filePath = do
  result <- P.readImage filePath
  case result of
    Right (P.ImageY8 img) -> do
      let ps = toList $ P.imageData img
      return $ Image (P.imageWidth img) (P.imageHeight img) ps
      -- I.runIL $ fmap arrayToImage $ I.readImage filePath
    Right _ -> error $ "Wrong image format: " ++ filePath
    Left msg -> error $ "Unable to read " ++ filePath ++ ": " ++ msg
  
writeImage :: FilePath -> Image -> IO ()
writeImage filePath img = do
  let ps = fromList $ pixels img :: Vector (P.PixelBaseComponent P.Pixel8)
  let img' = P.Image (iWidth img) (iHeight img) ps :: P.Image P.Pixel8
  -- let dImg = P.ImageY8 img'
  P.writePng filePath img'
  -- I.runIL $ I.writeImage filePath $ imageToArray img

-- imageToArray :: Image -> I.Image
-- imageToArray img = I.Grey arr
--   where w = iWidth img
--         h = iHeight img
--         xs = pixels img
--         arr = fromList ((Z :. h) :. w) xs

-- arrayToImage :: I.Image -> Image
-- arrayToImage (I.Grey arr) = Image w h $ toList arr
--   where (w:h:_) = listOfShape . extent $ arr
-- arrayToImage _ = error "Unsupported image type"

adjust :: [Word8] -> Double -> [Word8] -> [Word8]
adjust ts r xs
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | otherwise = zipWithCopyRight (adjust' r) ts xs

adjust' :: Double -> Word8 -> Word8 -> Word8
adjust' r t x = round(x' + r*(t' - x'))
  where t' = fromIntegral t :: Double
        x' = fromIntegral x :: Double

-- Zip until we run out of elements on the left, then copy elements from
-- the right
zipWithCopyRight :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithCopyRight f (x:xs) (y:ys)
  = f x y : zipWithCopyRight f xs ys
zipWithCopyRight _ _ [] = []
zipWithCopyRight _ [] ys = ys

-- imageDistance
--   :: (Integral a, Integral a1, Shape sh, Shape sh1, Source r a,
--       Source r1 a1) =>
--      Array r1 sh1 a1 -> Array r sh a -> Double
-- imageDistance a b = avg . map abs . zipWith (-) a' $ b'
--   where a' = map fromIntegral . toList $ a :: [Double]
--         b' = map fromIntegral . toList $ b :: [Double]

-- adjust
--   :: (Shape sh, Source r1 Word8, Source r2 Word8, Target r1 Word8) =>
--      Array r1 sh Word8
--      -> Double -> Array r2 sh Word8 -> Array r1 sh Word8
-- adjust target r a
--   | r < 0     = error "Negative learning rate"
--   | r > 1     = error "Learning rate > 1"
--   | otherwise = runIdentity . computeP $ R.zipWith (adjust' r) target a

-- data Array3D = Array3D (Array F DIM3 Word8) deriving (Eq, Generic)

-- data Array2D = Array2D (Array F DIM2 Word8) deriving (Eq, Generic)

-- instance Serialize Array3D where
--   put arr = S.put dims >> S.put (toList arr)
--     where dims = map fromIntegral . listOfShape . extent $ arr :: [Word8]
--   get = sGetArray 3

-- instance Serialize Array2D where
--   put arr = S.put dims >> S.put (toList arr)
--     where dims = map fromIntegral . listOfShape . extent $ arr :: [Word8]
--   get = sGetArray 2

-- instance Genetic Array3D where
--   put arr = put dims >> put (toList arr)
--     where dims = map fromIntegral . listOfShape . extent $ arr :: [Word8]
--   get = getArray 3

-- instance Genetic Array2D where
--   put arr = put dims >> put (toList arr)
--     where dims = map fromIntegral . listOfShape . extent $ arr :: [Word8]
--   get = getArray 2

-- instance Show Image where
--   show (RGBA a) = "RGB " ++ show (toList a)
--   show (RGB a)  = "RGB " ++ show (toList a)
--   show (BGRA a) = "RGB " ++ show (toList a)
--   show (BGR a)  = "RGB " ++ show (toList a)
--   show (Grey a) = "RGB " ++ show (toList a)
    
-- pixelCount :: Image -> Int
-- pixelCount (RGBA a) = length . toList $ a
-- pixelCount (RGB a)  = length . toList $ a
-- pixelCount (BGRA a) = length . toList $ a
-- pixelCount (BGR a)  = length . toList $ a
-- pixelCount (Grey a) = length . toList $ a

-- instance Pattern Image where
--   type Metric Image = Double
--   difference (Image a) (Image b) = imageDistance a b
--   -- difference (RGBA a) (RGBA b) = imageDistance a b
--   -- difference (RGB a) (RGB b) = imageDistance a b
--   -- difference (BGRA a) (BGRA b) = imageDistance a b
--   -- difference (BGR a) (BGR b) = imageDistance a b
--   -- difference (Grey a) (Grey b) = imageDistance a b
--   -- difference _ _ = 1.0 -- mismatched image types
--   makeSimilar (Image target) amount (Image a) = Image $ adjust target amount a
--   -- makeSimilar (RGBA target) amount (RGBA a) = RGBA $ adjust target amount a
--   -- makeSimilar (RGB target) amount (RGB a) = RGB $ adjust target amount a
--   -- makeSimilar (BGRA target) amount (BGRA a) = BGRA $ adjust target amount a
--   -- makeSimilar (BGR target) amount (BGR a) = BGR $ adjust target amount a
--   -- makeSimilar (Grey target) amount (Grey a) = Grey $ adjust target amount a
--   -- makeSimilar _ _ p = p -- mismatched image types

-- sGetShape :: Shape sh => Int -> Get sh
-- sGetShape nDims = do
--   dims <- S.get :: Get [Word8]
--   if length dims < nDims
--     then return $ error "Short dimension list"
--     else return . shapeOfList . map fromIntegral . take nDims $ dims
  
-- getShape :: Shape sh => Int -> Reader (Maybe sh)
-- getShape nDims = do
--   dims <- get :: Reader (Maybe [Word8])
--   case dims of
--     (Just ds) ->
--       if length ds < nDims
--         then return Nothing
--         else return . Just . shapeOfList . map fromIntegral . take nDims $ ds
--     Nothing -> return Nothing
  
-- sGetPixels :: Int -> Get [Word8]
-- sGetPixels n = do
--   pixels <- S.get :: Get [Word8]
--   if length pixels < n
--     then return $ error "Too few pixels"
--     else return $ pixels

-- getPixels :: Int -> Reader (Maybe [Word8])
-- getPixels n = do
--   pixels <- fmap (fmap (take n)) get :: Reader (Maybe [Word8])
--   case pixels of
--     (Just ps) -> 
--       if length ps < n
--         then return Nothing
--         else return . Just $ ps
--     Nothing -> return Nothing

-- sGetArray :: Shape sh => Int -> Get (Array F sh Word8)
-- sGetArray nDims = do
--   shape <- sGetShape nDims
--   pixels <- sGetPixels (size shape)
--   return $ fromList shape pixels
  
-- getArray :: Shape sh => Int -> Reader (Maybe (Array F sh Word8))
-- getArray nDims = do
--   shape <- getShape nDims
--   case shape of
--     (Just sh) -> do
--       pixels <- getPixels (size sh)
--       return $ fromList sh <$> pixels
--     Nothing -> return Nothing

-- imgExpress
--   :: (Shape sh, Source r1 Word8, Source r2 Word8, Target r1 Word8) =>
--      Array r1 sh Word8 -> Array r2 sh Word8 -> Array r1 sh Word8
-- imgExpress a b = runIdentity . computeP $ R.zipWith min a b

-- instance Diploid Image where
--   express (Image a) (Image b) = Image $ zipWith min a b
  -- express (RGBA a) (RGBA b) = RGBA $ imgExpress a b
  -- express (RGB a) (RGB b) = RGB $ imgExpress a b
  -- express (BGRA a) (BGRA b) = BGRA $ imgExpress a b
  -- express (BGR a) (BGR b) = BGR $ imgExpress a b
  -- express (Grey a) (Grey b) = Grey $ imgExpress a b
  -- express (RGBA a) _ = RGBA a
  -- express _ (RGBA b) = RGBA b
  -- express (RGB a) _ = RGB a
  -- express _ (RGB b) = RGB b
  -- express (BGRA a) _ = BGRA a
  -- express _ (BGRA b) = BGRA b
  -- express (BGR a) _ = BGR a
  -- express _ (BGR b) = BGR b

-- img <- runIL $ readImage "/home/amy/Downloads/wordle.png"
-- img2 <- runIL $ readImage "/home/amy/Downloads/wordle2.png"
-- let img3 = express img img2
-- runIL $ writeImage "/home/amy/Downloads/wordle3.png" img3
