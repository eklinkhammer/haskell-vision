{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleInstances #-}

module RobotVision.Display.Overlay.Class
(     overlayPoints
    , overlayPointsFaster
) where

import Data.Array.Repa
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Data.Array.Repa.Repr.ForeignPtr          as F
import Data.Word
import Prelude hiding (map)

setRGBPoint :: (Source r e, Num e) => (e, e, e) -> DIM2 -> Array r DIM3 e -> Array D DIM3 e
setRGBPoint  (rc, gc, bc) !(Z :. y :. x) img = fromFunction (extent img) f
    where
        f (Z :. j :. i :. k) 
            | (i == x) && (j == y) = case k of
                                    0 -> rc
                                    1 -> gc
                                    2 -> bc
            | otherwise = img ! (Z :. j :. i :. k)
{-# INLINE setRGBPoint #-}

-- Ideally, use points to just mutated state
-- Next best, loop over image and loop over points for each pixel (l*w*p)
-- Current implementation, loop over image for each pixel
-- Consider defining a function where, for a point in the DIM2 array, the value is red, else it 
--  is something else <<<-----
overlayPoints :: (Source r e, Num e) => (e,e,e) -> [DIM2] -> Array r DIM3 e -> Array D DIM3 e
overlayPoints _ [] !img = delay img
overlayPoints pix (!(Z :. y :. x):pts) !img = 
    let withPoint = setRGBPoint pix (Z :. y :. x) img
    in overlayPoints pix pts withPoint
{-# INLINE overlayPoints #-}

testSetPoint :: (Source r e, Num e) => Array r DIM3 e -> Array D DIM3 e
testSetPoint = setRGBPoint (255, 0, 0) (Z :. 50 :. 0)

-- 
overlayPointsFaster :: (Source r e, Num e) => (e,e,e) -> [DIM2] -> Array r DIM3 e -> Array D DIM3 e
overlayPointsFaster (r,g,b) !pts !baseImg = fromFunction (extent baseImg) f
  where
    f (Z :. j :. i :. k)
      | pt `elem` pts = case k of
                        0 -> r
                        1 -> g
                        2 -> b
      | otherwise = baseImg ! (Z :. j :. i :. k)
        where
          pt = Z :. j :. i
          pt1 = Z :. (clamp height (j-1)) :. (clamp width (i-1))
          !(Z :. height :. width :. _) = (extent baseImg)

clamp bound = max 0 . min bound 