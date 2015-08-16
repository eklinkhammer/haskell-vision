{-# LANGUAGE BangPatterns #-}

module Warping.Segment.Class
(       Segment (..)
    ,   Point (..)
    ,   segGetU -- "tested"
    ,   segGetV -- "tested"
    ,   uvToX
    ,   distPointToSegment -- "tested"
    ,   weight
    ,   pointDot -- Tested
    ,   segScalarMult -- Old
    ,   pointScalarMult -- Tested
    ,   segPerpUnit -- New
    ,   segPerp -- New
    ,   pointUnit -- New
    ,   segLength -- New
    ,   pointSubtract -- Tested
    ,   segSubtract -- Old
    ,   segWithPoint -- New
    ,   pointPerp -- Tested
    ,   floatMult -- Old (Implicitly tested)
    ,   segToPoints -- New
    ,   segPointDiff -- New
    ,   pointNorm -- Tested
    ,   square -- Tested
    ,   pointAdd
) where

data Segment = Segment {
      segX1  :: {-# UNPACK #-} !Int, segY1 :: {-# UNPACK #-} !Int
    , segX2  :: {-# UNPACK #-} !Int, segY2 :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)

data Point = Point {
      px  :: {-# UNPACK #-} !Int, py :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)

type Vector = Point -- not used, but makes sublime coloring look pretty. Also, consider changing to use this

segToPoints :: Segment -> (Point, Point)
segToPoints (Segment px py qx qy) = ((Point px py),(Point qx qy))

segPointDiff :: Segment -> Point
segPointDiff segment = let (p,q) = segToPoints segment in pointSubtract q p

pointSubtract :: Point -> Point -> Point
pointSubtract !(Point x1 y1) !(Point x2 y2) = Point (x1 - x2) (y1 - y2)

pointAdd :: Point -> Point -> Point
pointAdd !(Point x1 y1) !(Point x2 y2) = Point (x1 + x2) (y1 + y2)

pointPerp :: Point -> Point
pointPerp !(Point x y) = Point ((-1) * y) x 

pointDot :: Point -> Point -> Int
pointDot !(Point x1 y1) !(Point x2 y2) = (x1 * x2) + (y1 * y2)

pointSelfDot :: Point -> Int
pointSelfDot pt = pointDot pt pt

pointScalarMult :: Float -> Point -> Point
pointScalarMult f !(Point x1 y1) = let multF = floatMult f in Point (multF x1) (multF y1)

segScalarMult :: Float -> Segment -> Segment
segScalarMult f !(Segment x1 y1 x2 y2) = let multF = floatMult f in Segment (multF x1) (multF y1) (multF x2) (multF y2)

floatMult :: Float -> Int -> Int
floatMult f = round . ((*) f) . fromIntegral

pointNorm :: Point -> Float
pointNorm = sqrt . fromIntegral . pointSelfDot

square :: Int -> Int
square = flip (^) 2

segLength :: Segment -> Float
segLength = pointNorm . segPointDiff

pointsToSegment :: (Point, Point) -> Segment
pointsToSegment ((Point px py),(Point qx qy)) = Segment px py qx qy

segWithPoint :: Segment -> Point -> Segment
segWithPoint segment pt = let (p,_) = segToPoints segment in pointsToSegment (p,pt)

segSubtract :: Segment -> Segment -> Segment
segSubtract !(Segment x1 y1 x2 y2) !(Segment a1 b1 a2 b2) = Segment (a1 - x1) (b1 - y1) (a2 - x2) (b2 - y2)

-- Useless at moment
pointUnit :: Point -> Point
pointUnit pt = let norm = (fromIntegral . pointSelfDot) pt :: Float in pointScalarMult (1.0 / norm) pt

segPerpUnit :: Segment -> Point
segPerpUnit = pointUnit . segPerp

segPerp :: Segment -> Point
segPerp = pointPerp . segPointDiff

segGetU :: Segment -> Point -> Float
segGetU segment pt =
    let
        segVec = segPointDiff segment :: Point
        segPtVec = segPointDiff $ segWithPoint segment pt :: Point
        dotProdF = fromIntegral $ pointDot segPtVec segVec :: Float
        segL = fromIntegral $ pointSelfDot segVec :: Float
    in dotProdF / segL

segGetV :: Segment -> Point -> Float
segGetV segment pt =
    let
        segVec = segPointDiff $ segWithPoint segment pt :: Point
        perp = segPerp segment :: Point
        norm = segLength segment :: Float
    in (fromIntegral (pointDot perp segVec)) / norm

uvToX :: Segment -> Float -> Float -> Point
uvToX segPQ u v = 
    let
        vUnitPQ = pointScalarMult (v / segLength segPQ) (segPerp segPQ)
        uPQ = pointScalarMult u (segPointDiff segPQ)
        uAndV = pointAdd uPQ vUnitPQ
        (p,_) = segToPoints segPQ
    in pointAdd p uAndV

distPointToSegment :: Segment -> Point -> Float
distPointToSegment segmentPQ ptX
    | u > 0 && u < 1 = if v > 0 then v else ((-1) * v)
    | u <= 0 = segLength segPX
    | otherwise = segLength segQX
    where
        u = segGetU segmentPQ ptX
        v = segGetV segmentPQ ptX
        segPX = segWithPoint segmentPQ ptX
        segQX = let (_,q) = segToPoints segmentPQ in pointsToSegment (q, ptX)

weight :: Segment -> Point -> Float -> Float -> Float -> Float
weight segment pt a b p =
    let
        weightLength = (**) (segLength segment) p
        dist = distPointToSegment segment pt
        quot = weightLength / (a + dist)
    in (**) quot b