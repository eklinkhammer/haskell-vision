{-# LANGUAGE PackageImports, BangPatterns, FlexibleContexts, AllowAmbiguousTypes  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module RobotVision.ImageProcessing.ChannelProcessing
(       operateRGB
    , blurRGB
    , blurGrey
    , computeChannel
    , blurChannel
) where

import System.IO.Unsafe
import Data.Array.Repa
import Prelude                                  as P
import RobotVision.ImageRep.Utility (fromChannels, toChannels)
import Data.Vector.Unboxed.Base
import RobotVision.ImageProcessing.DoubleProcess (applyGauss)

operateRGB :: (Source r1 e1, Source r2 e2)
  => (Array D DIM2 e1 -> Array r2 DIM2 e2)
  -> Array r1 DIM3 e1 -> Array D DIM3 e2
operateRGB f = fromChannels . listToTuple . (P.map f) . tupleToList . toChannels

blurRGB :: (Source r Double) => Array r DIM3 Double -> Array D DIM3 Double
blurRGB = operateRGB blurGrey

blurGrey :: Array D DIM2 Double -> Array U DIM2 Double
blurGrey = blurChannel . computeChannel

blurChannel :: Array U DIM2 Double -> Array U DIM2 Double
blurChannel = unsafePerformIO . applyGauss

computeChannel ::  (Unbox u) => Array D DIM2 u -> Array U DIM2 u
computeChannel = unsafePerformIO . computeP

tupleToList :: (a,a,a) -> [a]
tupleToList (a,b,c) = [a,b,c]

listToTuple :: [a] -> (a,a,a)
listToTuple (x:y:z:[]) = (x,y,z)