{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module RobotVision.FeatureDetection.Feature
(     getFeature
    , Pixel
    , Feature
) where

import Data.Array.Repa as R hiding (map)
import Data.Word

data Pixel = Pixel {
      pt  :: DIM2, val :: Word8
    } deriving (Show, Read, Eq, Ord)

type Feature = [Pixel]

getFeature :: (Source r Word8) => Array r DIM2 Word8 -> DIM2 -> Feature
getFeature img pt = let pts = getSurroundingPoints pt in map (toPixel img) pts

toPixel :: (Source r Word8) => Array r DIM2 Word8 -> DIM2 -> Pixel
toPixel img pt = let val = img ! pt in Pixel pt val

-- Point is not an edge point by definition
getSurroundingPoints :: DIM2 -> [DIM2]
getSurroundingPoints (Z :. y :. x) = [topLeft, topMid, topRight, midLeft, midMid, midRight, botLeft, botMid, botRight]
  where
    topLeft = (Z :. (y + 1) :. (x - 1))
    topMid = (Z :. (y + 1) :. x)
    topRight = (Z :. (y + 1) :. (x + 1))
    midLeft = (Z :. y :. (x - 1))
    midMid = (Z :. y :. x)
    midRight = (Z :. y :. (x + 1))
    botLeft = (Z :. (y - 1) :. (x - 1))
    botMid = (Z :. (y - 1) :. x)
    botRight = (Z :. (y - 1) :. (x + 1))