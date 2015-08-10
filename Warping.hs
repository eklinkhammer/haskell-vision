{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies #-}

module Warping
(   test
) where

import Graphics.Gloss.Geometry.Line

test = id

-- Segment Functions
data Segment = Segment {
      segX1  :: {-# UNPACK #-} !Int, segY1 :: {-# UNPACK #-} !Int
    , segX2  :: {-# UNPACK #-} !Int, segY2 :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)

-- Point also represents 2D vector
data Point = Point {
      px  :: {-# UNPACK #-} !Int, py :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)

segGetU :: Segment -> Point -> Int
segGetU !(Segment x1 y1 x2 y2) (Point px py) =
    let 
        segPX = Segment x1 y1 px py
        distXY = segDist (Segment x1 y1 x2 y2)
        distPX = segDist segPX
        dotProd = pointDot distXY distPX
        distXY2 = pointDot distXY distXY
    in div dotProd distXY2

segGetV :: Segment -> Point -> Int
segGetV !(Segment x1 y1 x2 y2) (Point px py) =
    let
        segPX = Segment x1 y1 px py
        distPX = segDist segPX
        perpXYDivByXYLength = segToPerpNorm (Segment x1 y1 x2 y2)
    in pointDot distPX perpXYDivByXYLength

uvToX :: Segment -> Int -> Int -> Point
uvToX !(Segment x1 y1 x2 y2) u v =
    let
        distPQ = segDist (Segment x1 y1 x2 y2)
        x = 1 :: Int
        y = 2 :: Int
    in Point x y
segDot :: Segment -> Segment -> Int
segDot !(Segment x1 y1 x2 y2) !(Segment a1 b1 a2 b2)
    = (x1 * a1) + (y1 * b1) + (x2 * a2) + (y2 * b2)

pointDot :: Point -> Point -> Int
pointDot !(Point x1 y1) !(Point x2 y2) = (x1 * x2) + (y1 * y2)

segScalarMult :: Float -> Segment -> Segment
segScalarMult f !(Segment x1 y1 x2 y2)
    = let multF = floatMult f
      in Segment (multF x1) (multF y1) (multF x2) (multF y2)

pointScalarMult :: Float -> Point -> Point
pointScalarMult f !(Point x1 y1)
    = let multF = floatMult f
      in Point (multF x1) (multF y1)

segToPerpNorm :: Segment -> Point
segToPerpNorm !(Segment x1 y1 x2 y2) =
    let
        distXY = Point segVecX segVecY
        (Point segVecX segVecY) = segDist (Segment x1 y1 x2 y2)
        perpXY = Point ((-1) * segVecY) (segVecX)
        lengthXY = sqrt . fromIntegral $ pointDot distXY distXY :: Float
        scalar = 1.0 / lengthXY :: Float
    in pointScalarMult scalar perpXY

floatMult :: Float -> Int -> Int
floatMult f = round . ((*) f) . fromIntegral

pointSubtract :: Point -> Point -> Point
pointSubtract !(Point x1 y1) !(Point x2 y2) = Point (x2 - x1) (y2 - y1)

segSubtract :: Segment -> Segment -> Segment
segSubtract !(Segment x1 y1 x2 y2) !(Segment a1 b1 a2 b2) 
    = Segment (a1 - x1) (b1 - y1) (a2 - x2) (b2 - y2)

segDist :: Segment -> Point
segDist !(Segment x1 y1 x2 y2) = pointSubtract (Point x1 y1) (Point x2 y2)