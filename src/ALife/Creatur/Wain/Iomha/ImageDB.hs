------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Iomha.ImageDB
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A ridiculously simple database that stores each record in a
-- separate file. The name of the file is the record's key.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Iomha.ImageDB
  (
    ImageDB,
    mkImageDB,
    anyImage
  ) where

import ALife.Creatur.Wain.Iomha.Image (Image, readImage)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, put)
import System.Directory (getDirectoryContents)
import System.Random (randomRIO)

-- | A simple database where each record is stored in a separate file, 
--   and the name of the file is the record's key.
data ImageDB = ImageDB
  {
    initialised :: Bool,
    mainDir :: FilePath,
    images :: [FilePath],
    numImages :: Int
  } deriving (Eq)

instance Show ImageDB where
  show = mainDir

-- | @'mkImageDB' d@ (re)creates the ImageDB in the
--   directory @d@.
mkImageDB :: FilePath -> ImageDB
mkImageDB d = ImageDB False d undefined undefined

initIfNeeded :: StateT ImageDB IO ()
initIfNeeded = do
  isInitialised <- gets initialised
  unless isInitialised $ do
    db <- get
    db' <- liftIO $ initialise db
    put db'

initialise :: ImageDB -> IO ImageDB
initialise db = do
  files <- liftIO . getDirectoryContents . mainDir $ db
  let imageFiles = filter isImageFileName files
  return db { initialised=True, images=imageFiles,
             numImages=length imageFiles }

isImageFileName :: String -> Bool
isImageFileName s =
  s `notElem` [ "archive", ".", ".." ]

anyImage :: StateT ImageDB IO (Image, String)
anyImage = do
  initIfNeeded
  db <- get
  k <- liftIO $ randomRIO (0,numImages db - 1)
  let filename = images db !! k
  img <- liftIO . readImage $ mainDir db ++ '/':filename
  return (img, filename)
