{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleInstances
           , TemplateHaskell, QuasiQuotes
           , TypeOperators
           , AllowAmbiguousTypes
            #-}

module RobotVision.FeatureDetection.HarrisCorners
(     harrisCorners
    , cornerImage
    , getDerivativeImages
    , tensorImage
    , cornerResponse
) where

import RobotVision.ImageRep.Class
import RobotVision.ImageRep.Utility
import RobotVision.Display.Overlay.Class
import Prelude as P
import Data.Array.Repa as R
import Data.Word
import RobotVision.ImageProcessing.DoubleProcess
import RobotVision.ImageProcessing.ChannelProcessing
import System.IO.Unsafe
import Data.Array.Repa.IO.DevIL

test = id

--data Pixel = Pixel {
--      pt  :: DIM2, val :: Word8
--    } deriving (Show, Read, Eq, Ord)

--type Feature r = Array r DIM1 Pixel

--getFeatures :: (Source r Word8) => Array r DIM2 Word8 -> [DIM2] -> [Feature]
--getFeatures img = P.map (getFeature img)

--getFeature :: (Source r Word8) => Array r DIM2 Word8 -> DIM2 -> Feature
--getFeature img (Z :. y :. x) = fromFunction (Z :. 9) getPixels
--  let
--    topLeft = (Z :. (y + 1) :. (x - 1))
--    topMid = (Z :. (y + 1) :. x)
--    topRight = (Z :. (y + 1) :. (x + 1))
--    midLeft = (Z :. y :. (x - 1))
--    midMid = (Z :. y :. x)
--    midRight = (Z :. y :. (x + 1))
--    botLeft = (Z :. (y - 1) :. (x - 1))
--    botMid = (Z :. (y - 1) :. x)
--    botRight = (Z :. (y - 1) :. (x + 1)
--    getPixels (Z :. i)
--  in fromFunction (Z :. 9) getPixels
--    where 
--      | i == 0 = Pixel topLeft (img ! topLeft)
--      | i == 1 = Pixel topMid (img ! topMid)
--      | i == 2 = Pixel topRight (img ! topRight)
--      | i == 3 = Pixel midLeft (img ! midLeft)
--      | i == 4 = Pixel midMid (img ! midMid)
--      | i == 5 = Pixel midRight (img ! midRight)
--      | i == 6 = Pixel botLeft (img ! botLeft)
--      | i == 7 = Pixel botMid (img ! botMid)
--      | i == 8 = Pixel botRight (img ! botRight)




cornerImage :: (Source r Word8) => Array r DIM3 Word8 -> Array D DIM3 Word8
cornerImage img = 
    let
        rgb = toRGB img :: Array D DIM3 Word8
        grey = toGrey rgb :: Array D DIM2 Word8
        greyI = expand grey :: Array D DIM2 Int
        greyUnboxed = computeChannel (R.map (toDouble) greyI) :: Array U DIM2 Double
        points = harrisCorners greyUnboxed :: [DIM2]
    in overlayPointsFaster (255,0,0) points rgb

harrisCorners :: Array U DIM2 Double -> [DIM2]
harrisCorners img = 
    let
        blur = blurChannel img :: Array U DIM2 Double
        gradImgs = getDerivativeImages blur :: (Array U DIM2 Double, Array U DIM2 Double)
        tensorImg = tensorImage gradImgs :: Array D DIM3 Double
        potentialCorners = getNotBoundaryPoints img :: [DIM2]
        positive_responses = filter isCorner $ P.map (cornerResponse 0.15 tensorImg) potentialCorners
        average = sum positive_responses / (fromIntegral $ length positive_responses :: Double)
    in filter ((<) 2 . (\x -> x / average) . cornerResponse 0.15 tensorImg) potentialCorners

cornerResponse :: Double -> Array D DIM3 Double -> DIM2 -> Double
cornerResponse k tensorImg (Z :. y :. x) = det - k * (square trace)
  where
    trace = dx2 + dy2
    det = dx2 * dy2 - (square dxy)
    dx2 = tensorImg ! (Z :. y :. x :. 0)
    dy2 = tensorImg ! (Z :. y :. x :. 2)
    dxy = tensorImg ! (Z :. y :. x :. 1)
{-# INLINE cornerResponse #-}

tensorImage :: (Array U DIM2 Double, Array U DIM2 Double) -> (Array D DIM3 Double)
tensorImage (dx, dy) =
  let
    (Z :. height :. width) = extent dx
  in blurRGB $ fromFunction (Z :. height :. width :. 3) f
    where
      f (Z :. j :. i :. k) = case k of
                              0 -> dx2
                              1 -> dxdy
                              2 -> dy2
        where
          dy2 = dyVal * dyVal
          dxdy = dxVal * dyVal
          dx2 = dxVal * dxVal
          dxVal = dx ! (Z :. j :. i)
          dyVal = dy ! (Z :. j :. i)
{-# INLINE tensorImage #-}

getDerivativeImages :: Array U DIM2 Double -> (Array U DIM2 Double, Array U DIM2 Double)
getDerivativeImages img = (dx,dy)
    where
        dx = unsafePerformIO $ applyDx img
        dy = unsafePerformIO $ applyDy img

square :: (Num a) => a -> a
square x = (*) x x

isCorner :: Double -> Bool
isCorner = (<) 0

toDouble :: (Real n) => n -> Double
toDouble = realToFrac