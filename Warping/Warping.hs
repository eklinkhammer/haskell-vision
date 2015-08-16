{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies #-}

module Warping.Warping
(       warpImage
    ,   Segment (..)
) where

import Warping.Segment as W
import Vision.Image
import Prelude as P
import Data.RatioInt
import Vision.Primitive
import Data.Word


test = id
 


warpImage :: (Image i1, FromFunction i2
                  , ImagePixel i1 ~ FromFunctionPixel i2
                  , Interpolable (FromFunctionPixel i2)
                  , Integral (PixelChannel (FromFunctionPixel i2)))
               => [Segment] -> [Segment] -> Maybe Float -> Maybe Float -> Maybe Float -> i1 -> i2
warpImage segsBefore segsAfter a b p img
    | length segsAfter /= length segsBefore = error "Must be same number of before and after segments."
    | length segsAfter > 1  = warp segsBefore segsAfter (a // 10) (b // 1) (p // 1) img
    | otherwise             =  warpBy1 (head segsBefore) (head segsAfter) img

(//) :: Maybe a -> a -> a
Just x  // _ = x
Nothing // y = y

warpBy1 :: (Image i1, FromFunction i2
                  , ImagePixel i1 ~ FromFunctionPixel i2
                  , Interpolable (FromFunctionPixel i2)
                  , Integral (PixelChannel (FromFunctionPixel i2)))
               => W.Segment -> W.Segment -> i1 -> i2
warpBy1 segBefore segAfter !img =
    let f !(Z :. y' :. x') = let !x = bound maxWidth $ ratio access_x :: RatioInt
                                 !y = bound maxHeight $ ratio access_y :: RatioInt
                                 maxHeight = ratio (w-1)
                                 maxWidth = ratio (h-1)
                            in img `bilinearInterpol` RPoint y x
            where
                u_after = W.segGetU segAfter (W.Point y' x')
                v_after = W.segGetV segAfter (W.Point y' x')
                (W.Point access_x access_y) = uvToXSeg u_after v_after
        {-# INLINE f #-}
    in fromFunction size f
    where
        !uvToXSeg = W.uvToX segBefore
        bound !limit = min limit . max 0
        !size@(Z :. h :. w) = shape img


warp :: (Image i1, FromFunction i2
                  , ImagePixel i1 ~ FromFunctionPixel i2
                  , Interpolable (FromFunctionPixel i2)
                  , Integral (PixelChannel (FromFunctionPixel i2)))
               => [Segment] -> [Segment] -> Float -> Float -> Float -> i1 -> i2
warp segsBefore segsAfter a b p !img =
    let f !(Z :. y' :. x') = let !x = bound maxWidth $ ratio access_x :: RatioInt
                                 !y = bound maxHeight $ ratio access_y :: RatioInt
                                 maxHeight = ratio (w-1)
                                 maxWidth = ratio (h-1)
                            in img `bilinearInterpol` RPoint y x
            where
                d_sum = getDSum segsBefore segsAfter ptX' a b p
                (W.Point access_x access_y) = W.pointAdd d_sum ptX'
                ptX' = Point y' x'
        {-# INLINE f #-}
    in fromFunction size f
    where
        bound !limit = min limit . max 0
        !size@(Z :. h :. w) = shape img


getDSum :: [W.Segment] -> [W.Segment] -> W.Point -> Float -> Float -> Float -> W.Point
getDSum segsBefore segsAfter ptX a b p = dSum dispAndWeights
    where
        dispAndWeights = P.zipWith (getDisplacementAndWeight ptX a b p) segsBefore segsAfter :: [(Float, W.Point)]
        dSum = (sumPoints . weightedPoints . normWeights weightsum)
        weightsum = sumFirst dispAndWeights

sumFirst :: [(Float, a)] -> Float
sumFirst = sum . P.map fst

sumPoints :: [W.Point] -> W.Point
sumPoints = P.foldl W.pointAdd (W.Point 0 0)

normWeights :: Float -> [(Float, W.Point)] -> [(Float, W.Point)]
normWeights norm = P.map (\(f,p) -> (f / norm, p))

weightedPoints :: [(Float, W.Point)] -> [W.Point]
weightedPoints = P.map (P.uncurry $ W.pointScalarMult)

getDisplacementAndWeight :: W.Point -> Float -> Float -> Float -> W.Segment -> W.Segment -> (Float, W.Point)
getDisplacementAndWeight ptX a b p segBefore segAfter = (weightS, displacement)
    where
        weightS = W.weight segAfter ptX a b p
        displacement = W.pointSubtract ptXP ptX
        ptXP = W.uvToX segBefore u v
        u = W.segGetU segAfter ptX
        v = W.segGetV segAfter ptX

ratio :: Integral a => a -> RatioInt
ratio = fromIntegral


distImage :: (Image i1) => Segment -> i1 -> Grey
distImage segment !img =
    let f !(Z :. y' :. x') = let !x = x'
                                 !y = y'
                                 val = round $ distPointToSegment segment (Point x' y') :: Word8
                            in GreyPixel val
        {-# INLINE f #-}
    in fromFunction size f
    where
        !size@(Z :. h :. w) = shape img