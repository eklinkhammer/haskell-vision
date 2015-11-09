{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module RobotVision.ImageProcessing.RepaExamples
(       processImage
    ,   Filter
    ,   applyStencil
    ,   applyStencilClamped
    ,   applyDy
    ,   applyDx
    ,   applyGauss
) where
import Control.Monad
import Data.Word
import Data.Array.Repa                          as A
import Data.Array.Repa.Stencil                  as A
import Data.Array.Repa.Stencil.Dim2             as A
import Prelude                                  as P

type Filter = Array U DIM2 Double -> IO (Array U DIM2 Double)

applyStencil :: Stencil DIM2 Double -> Filter
applyStencil stencil = computeP . mapStencil2 (BoundConst 0) stencil

applyStencilClamped :: Stencil DIM2 Double -> Filter
applyStencilClamped stencil = computeP . mapStencil2 BoundClamp stencil

applyDx :: Array U DIM2 Int -> IO (Array U DIM2 Int)
applyDx = processImageSignMattered derivXFilter

applyDy :: Array U DIM2 Int -> IO (Array U DIM2 Int)
applyDy = processImageSignMattered derivYFilter

applyGauss :: Array U DIM2 Int -> IO (Array U DIM2 Int)
applyGauss = processImageSignMattered gaussFilter

processImage :: Filter -> Array U DIM2 Word8 -> IO (Array U DIM2 Word8)
processImage filter = promote >=> filter >=> demote
        
processImageSignMattered :: Filter -> Array U DIM2 Int -> IO (Array U DIM2 Int)
processImageSignMattered filter = promoteSignMattered >=> filter >=> demoteSignMattered

promote :: Monad m => Array U DIM2 Word8 -> m (Array U DIM2 Double)
promote arr
 = computeP $ A.map ffs arr
 where  {-# INLINE ffs #-}
        ffs     :: Word8 -> Double
        ffs x   =  fromIntegral (fromIntegral x :: Int)
{-# NOINLINE promote #-}


demote  :: Monad m => Array U DIM2 Double -> m (Array U DIM2 Word8)
demote arr
 = computeP $ A.map ffs arr

 where  {-# INLINE ffs #-}
        ffs     :: Double -> Word8
        ffs x   =  fromIntegral (truncate x :: Int)
{-# NOINLINE demote #-}

promoteSignMattered :: Monad m => Array U DIM2 Int -> m (Array U DIM2 Double)
promoteSignMattered arr
 = computeP $ A.map ffs arr
 where  {-# INLINE ffs #-}
        ffs     :: Int -> Double
        ffs   =  fromIntegral
{-# NOINLINE promoteSignMattered #-}
-- When the sign mattered (as with derivatives) we limit the range to -128->127
-- and add 128. This means the new range is from 0->255
demoteSignMattered :: Monad m => Array U DIM2 Double -> m (Array U DIM2 Int)
demoteSignMattered arr
 = computeP $ A.map ffs arr

 where  {-# INLINE ffs #-}
        ffs     :: Double -> Int
        ffs =  truncate
{-# NOINLINE demoteSignMattered #-}

gaussStencil :: Stencil DIM2 Double
gaussStencil =
        [stencil2| 2 4 5 4 2
                   4 9 12 9 4
                   5 12 15 12 5
                   4 9 12 9 4
                   2 4 5 4 2 |]

normalize :: Double -> Filter
normalize n = computeP . A.map (/ n)

gaussFilter :: Filter
gaussFilter = applyStencil gaussStencil >=> normalize 159

edgeStencil :: Stencil DIM2 Double
edgeStencil =
    [stencil2| 0 1 0
               1 -4 1
               0 1 0 |]

edgeFilter :: Filter
edgeFilter = applyStencilClamped edgeStencil

derivativeX :: Stencil DIM2 Double
derivativeX =
    [stencil2| (-1) 0 1
               (-2) 0 2
               (-1) 0 1 |]

derivativeY :: Stencil DIM2 Double
derivativeY =
    [stencil2| (-1) (-2) (-1)
               0 0 0
               1 2 1 |]

derivXFilter :: Filter
derivXFilter = applyStencilClamped derivativeX

derivYFilter :: Filter
derivYFilter = applyStencilClamped derivativeY


                        