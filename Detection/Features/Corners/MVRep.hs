module MVRep
(       test
    ,   expand
) where

import Data.Word
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Data.Array.Repa                          as A
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Data.Array.Repa.Stencil                  as A
import Data.Array.Repa.Stencil.Dim2             as A
import Prelude                                  as P
import Data.Array.Repa.Repr.ForeignPtr          as F

test = id

expand :: Array U DIM3 Word8 -> Array U DIM3 Int
expand arr = fromFunction (extent arr) fromIntegral arr


toTuple ::  Array U DIM3 Word8 -> (Array U DIM2 Word8, Array U DIM2 Word8, Array U DIM2 Word8)
toTuple arr = (red, green, blue)
    where
        red = unsafePerformIO $ computeP $ fromFunction (Z:. x :. y) f
        green = unsafePerformIO $ computeP $ fromFunction (Z:. x :. y) g
        blue = unsafePerformIO $ computeP $ fromFunction (Z:. x :. y) h
        sh@(Z :. x :. y :. _) = extent arr
        f (Z :. i :. j) = arr ! (Z :. i :. j :. 0)
        g (Z :. i :. j) = arr ! (Z :. i :. j :. 1)
        h (Z :. i :. j) = arr ! (Z :. i :. j :. 2)