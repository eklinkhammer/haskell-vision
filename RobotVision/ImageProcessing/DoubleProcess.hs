{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module RobotVision.ImageProcessing.DoubleProcess
(       --processImage
        Filter
    ,   applyStencil
    ,   applyStencilClamped
    ,   applyDy
    ,   applyDx
    ,   applyGauss
    ,   demote
) where
import Control.Monad
import Data.Array.Repa                          as A
import Data.Array.Repa.Stencil                  as A
import Data.Array.Repa.Stencil.Dim2             as A
import Prelude                                  as P
import Data.Vector.Unboxed.Base
import Data.Word

type Filter = Array U DIM2 Double -> IO (Array U DIM2 Double)

applyStencil :: Stencil DIM2 Double -> Filter
applyStencil stencil = computeP . mapStencil2 (BoundConst 0) stencil

applyStencilClamped :: Stencil DIM2 Double -> Filter
applyStencilClamped stencil = computeP . mapStencil2 BoundClamp stencil

applyDx :: Array U DIM2 Double -> IO (Array U DIM2 Double)
applyDx = derivXFilter

applyDy :: Array U DIM2 Double -> IO (Array U DIM2 Double)
applyDy = derivYFilter

applyGauss :: Array U DIM2 Double -> IO (Array U DIM2 Double)
applyGauss = gaussFilter

--processImage :: Filter -> Array U DIM2 Double -> IO (Array U DIM2 Double)
--processImage f = promote >=> f

--promote :: (Monad m, Unbox n, Real n) => Array U DIM2 n -> m (Array U DIM2 Double)
--promote arr
-- = computeP $ A.map ffs arr
-- where  {-# INLINE ffs #-}
--        ffs     :: (Real n) => n -> Double
--        ffs     =  realToFrac
--{-# NOINLINE promote #-}

demote :: (Source r Double) => Array r DIM2 Double -> Array D DIM2 Word8
demote = A.map (toWord)

toWord :: Double -> Word8
toWord = intToWord . toInt

toInt :: Double -> Int
toInt = round

intToWord :: Int -> Word8
intToWord = fromIntegral

gaussStencil :: Stencil DIM2 Double
gaussStencil =
        [stencil2| 2 4 5 4 2
                   4 9 12 9 4
                   5 12 15 12 5
                   4 9 12 9 4
                   2 4 5 4 2 |]

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

gaussFilter :: Filter
gaussFilter = applyStencil gaussStencil >=> normalize 159

normalize :: Double -> Filter
normalize n = computeP . A.map (/ n)