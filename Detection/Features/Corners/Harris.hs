{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies
           , AllowAmbiguousTypes #-}

module Detection.Features.Corners.Harris
(       test
) where

import Vision.Image.Filter as F
import Vision.Image
import Vision.Primitive
import Foreign.Storable (Storable)
import Warping.Segment as W
import Prelude

valueImage :: (Image src, Integral (ImagePixel src),
    FromFunction res, Integral (FromFunctionPixel res),
    SeparatelyFiltrable src res (FromFunctionPixel res),
    ImagePixel src ~ FromFunctionPixel res)
    => (src, src) -> res
valueImage (dx, dy) = fromFunction (shape dx) f
    where f pt = getVal (dx ! pt) (dy ! pt)


getShapePoints :: (Image i) => i -> [DIM2]
getShapePoints = shapeList . shape

getVal :: (Integral a) => a -> a -> a
getVal dx dy = 
    let 
        (dx2, dy2) = (square dx, square dy)
        dxy' = dxy dx dy
        trc = trace dx2 dy2
        determ = det dx2 dy2 dxy'
    in (-) (determ * 10) ((square trc))

det :: (Num a) => a -> a -> a -> a
det dx2 dy2 dxy = (-) ((*) dx2 dy2) dxy

trace :: (Num a) => a -> a -> a
trace dx2 dy2 = (+) dx2 dy2  

square :: (Num a) => a -> a
square x = (*) x x

dxy :: (Num a) => a -> a -> a
dxy = (*)

derivatives :: (Image src, Integral (ImagePixel src),
    FromFunction res, Integral (FromFunctionPixel res),
    Storable (FromFunctionPixel res),
    SeparatelyFiltrable src res (FromFunctionPixel res))
    => src -> (res,res)
derivatives img = (xDeriv img, yDeriv img)

xDeriv :: (Image src, Integral (ImagePixel src),
    FromFunction res, Integral (FromFunctionPixel res),
    Storable (FromFunctionPixel res),
    SeparatelyFiltrable src res (FromFunctionPixel res))
    => src -> res
xDeriv = scharr DerivativeX

yDeriv :: (Image src, Integral (ImagePixel src),
    FromFunction res, Integral (FromFunctionPixel res),
    Storable (FromFunctionPixel res),
    SeparatelyFiltrable src res (FromFunctionPixel res))
    => src -> res
yDeriv = scharr DerivativeY


test = id