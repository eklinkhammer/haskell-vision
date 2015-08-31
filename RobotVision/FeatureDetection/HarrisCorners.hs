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
(     brighten
    , harrisCorners
    , cornerImage
    , blurChannel
    , blurGrey
    , getDerivativeImages
    , tensorImage
    , cornerResponse
    , getNotBoundaryPoints
    , getPtVal
) where

import RobotVision.ImageRep.Class
import RobotVision.Display.Overlay.Class
import Prelude as P
import Data.Array.Repa as R
import Data.Word
import RobotVision.ImageProcessing.RepaExamples
import System.IO.Unsafe
import Data.Array.Repa.IO.DevIL

test = id

expand' :: (Source r e, Shape sh, 
            Expandable (Array r sh e) (Array D sh Int)) 
            => Array r sh e -> Array D sh Int
expand' = expand

shrink' :: (Shape sh,
            Shrinkable (Array D sh Int) (Array D sh Word8))
            => Array D sh Int -> Array D sh Word8
shrink' = shrink

brighten :: (Source r e,
    Shape sh, 
    Expandable (Array r sh e) (Array D sh Int),
    Shrinkable (Array D sh Int) (Array D sh e))
    => Array r sh e -> Array D sh e
brighten = shrink . R.map (2 *) . expand




cornerImage :: (Source r Word8) => Array r DIM3 Word8 -> Array D DIM3 Word8
cornerImage img = 
    let
        rgb = toRGB img :: RGBDW
        grey = toGrey rgb :: GreyDW
        greyUnboxed = computeChannel grey :: GreyU
        points = harrisCorners greyUnboxed :: [DIM2]
    in overlayPoints (255,0,0) points rgb

-- No other image type should try to have corners found
-- Therefore, this function has a simple type signiture
-- Feature, not a bug
harrisCorners :: GreyU -> [DIM2]
harrisCorners img = 
    let
        gradImgs = getDerivativeImages (blurChannel img) :: (GreyU, GreyU)
        tensorImg = tensorImage gradImgs
        potentialCorners = getNotBoundaryPoints img :: [DIM2]
    in filter (isCorner . (cornerResponse 0.15 tensorImg)) potentialCorners

cornerResponse :: Double -> RGBDW -> DIM2 -> Double
cornerResponse k tensorImg (Z :. y :. x) = det - k * (square trace)
  where
    trace = dx2 + dy2
    det = dx2 * dy2 - dxy
    dx2 = fromIntegral $ tensorImg ! (Z :. y :. x :. 0) :: Double
    dy2 = fromIntegral $ tensorImg ! (Z :. y :. x :. 2) :: Double
    dxy = fromIntegral $ tensorImg ! (Z :. y :. x :. 1) :: Double

getPtVal :: (GreyU, GreyU) -> DIM2 -> Double
getPtVal (idx,idy) pt = 
    let
        dx = (fromIntegral (idx ! pt)) / 255 :: Double
        dy = (fromIntegral (idy ! pt)) / 255 :: Double
        dx2 = square dx
        dy2 = square dy
        dxy = (*) dx dy
        trace = (+) dx2 dy2
        det = (dx2 * dy2) - (square dxy)
    in (-) (det) ((*) 0.15 (square trace))


tensorImage :: (GreyU, GreyU) -> RGBDW
tensorImage (dx, dy) =
  let
    (Z :. height :. width) = extent dx
  in blurRGB $ fromFunction (Z :. height :. width :. 3) f
    where
      f (Z :. j :. i :. k) = case k of
                              0 -> fromIntegral $ max 0 $ min 255 $ dxVal * dxVal :: Word8
                              1 -> fromIntegral $ max 0 $ min 255 $ dxVal * dyVal :: Word8
                              2 -> fromIntegral $ max 0 $ min 255 $ dyVal * dyVal :: Word8
        where
          dxVal = fromIntegral $ dx ! (Z :. j :. i) :: Int
          dyVal = fromIntegral $ dy ! (Z :. j :. i) :: Int
{-# INLINE tensorImage #-}

blurRGB :: RGBDW -> RGBDW
blurRGB = fromChannels . listToTuple . (P.map blurGrey) . tupleToList . toChannels

blurGrey :: GreyDW -> GreyU
blurGrey = blurChannel . computeChannel

blurChannel :: GreyU -> GreyU
blurChannel = unsafePerformIO . applyGauss

computeChannel :: GreyDW -> GreyU
computeChannel = unsafePerformIO . computeP

tupleToList :: (a,a,a) -> [a]
tupleToList (a,b,c) = [a,b,c]

listToTuple :: [a] -> (a,a,a)
listToTuple (x:y:z:[]) = (x,y,z)

getNotBoundaryPoints :: GreyU -> [DIM2]
getNotBoundaryPoints img = notBoundaryPoints img $ getImagePoints img

getImagePoints :: GreyU -> [DIM2]
getImagePoints img = let (Z :. y :. x) = extent img in P.map tupleToPoint $ permuteList [0 .. (y-1)] [0 .. (x-1)] 

notBoundaryPoints :: GreyU -> [DIM2] -> [DIM2]
notBoundaryPoints img pts =
  let (Z :. w :. h) = extent img in filter (not . (onBoundary w h)) pts

onTop :: Int -> DIM2 -> Bool
onTop height (Z :. y :. _) = y == (height - 1)

onBot :: DIM2 -> Bool
onBot (Z :. y :. _) = y == 0

onLeft :: DIM2 -> Bool
onLeft (Z :. _ :. x) = x == 0

onRight :: Int -> DIM2 -> Bool
onRight width (Z :. _ :. x) = x == (width - 1)

onBoundary :: Int -> Int -> DIM2 -> Bool
onBoundary height width pt = (onTop height pt) || (onBot pt) || (onLeft pt) || (onRight width pt)

tupleList :: [a] -> b -> [(a,b)]
tupleList [] _ = []
tupleList (x:xs) y = (x,y) : (tupleList xs y)

permuteList :: [a] -> [b] -> [(a,b)]
permuteList _ [] = []
permuteList xs (y:ys) = tupleList xs y P.++ permuteList xs ys

tupleToPoint :: (Int, Int) -> DIM2
tupleToPoint (x,y) = Z :. x :. y

getDerivativeImages :: GreyU -> (GreyU, GreyU)
getDerivativeImages img = (dx,dy)
    where
        dx = unsafePerformIO $ applyDx img :: GreyU -- Find out what is proper to use here
        dy = unsafePerformIO $ applyDy img :: GreyU

square :: Double -> Double
square x = (*) x x

isCorner :: Double -> Bool
isCorner = (<) 0