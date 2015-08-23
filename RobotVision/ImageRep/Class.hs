{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleInstances #-}

module RobotVision.ImageRep.Class
(     Expandable (..)
    , Shrinkable (..)
    , GreyImage
    , RGBImage
    , Grey, GreyD, GreyU, GreyF, GreyI, GreyDW
    , RGB, RGBD, RGBU, RGBF, RGBI, RGBDW
    , Image
    , ToGrey (..)

) where

import Data.Array.Repa
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Data.Array.Repa.Repr.ForeignPtr          as F
import Data.Word
import Prelude hiding (map)
import Data.Convertible
test = id

-- Image Representation for this project is limited to, strictly, RGB, Grey, and HSV Word8, Int, and Float
-- The only representations that are permissible in memory are the Word8 ones. Because Haskell can inline
-- the conversion functions and this library makes use of Repa's delayed computation capabilities, the conversions
-- happen as needed, in place, but are never stored to avoid type change induced copying.

-- To avoid confusing types, all functions will be explicit. Generalizing types does not (I think) make sense
-- in this context. All computation itself will be generalized, but types will be explicit.

--- Origin is at bottom
-- (y,0)            (y,x)







-- (0,0)            (0,x)


type Grey r e = Array r DIM2 e
type GreyD e = Grey D e
type GreyU = Grey U Word8
type GreyF = Grey F Word8

type RGB r e = Array r DIM3 e
type RGBD e = RGB D e
type RGBU = RGB U Word8
type RGBF = RGB F Word8

type GreyI = Grey D Int
type RGBI = RGB D Int
type GreyDW = Grey D Word8
type RGBDW = RGB D Word8

data GreyImage = GreyI | GreyU | GreyF | GreyDW 
data RGBImage = RGBI | RGBU | RGBF | RGBDW
data Image = GreyImage | RGBImage

class Expandable a b | a -> b where
    expand :: a -> b

instance Expandable GreyU GreyI where
    expand = map fromIntegral
    {-# INLINE expand #-}

instance Expandable GreyF GreyI where
    expand = map fromIntegral
    {-# INLINE expand #-}

instance Expandable GreyDW GreyI where
    expand = map fromIntegral
    {-# INLINE expand #-}

instance Expandable GreyI GreyI where
    expand = id
    {-# INLINE expand #-}

instance Expandable RGBU RGBI where
    expand = map fromIntegral
    {-# INLINE expand #-}

instance Expandable RGBF RGBI where
    expand = map fromIntegral
    {-# INLINE expand #-}

instance Expandable RGBDW RGBI where
    expand = map fromIntegral
    {-# INLINE expand #-}

instance Expandable RGBI RGBI where
    expand = id
    {-# INLINE expand #-}

class Shrinkable a b | a -> b where
    shrink :: a -> b

instance Shrinkable GreyI GreyDW where
    shrink = map (fromIntegral . max 0 . min 255)
    {-# INLINE shrink #-}

instance Shrinkable GreyDW GreyDW where
    shrink = id
    {-# INLINE shrink #-}

instance Shrinkable RGBI RGBDW where
    shrink = map (fromIntegral . max 0 . min 255)
    {-# INLINE shrink #-}

instance Shrinkable RGBDW RGBDW where
    shrink = id
    {-# INLINE shrink #-}

class ToGrey a b where
    toGrey :: a -> b

instance ToGrey RGBDW GreyDW where
  toGrey = combineImage rgbToGrey
  {-# INLINE toGrey #-}

-- Takes a function to convert one rgb pixel into one grey pixel
combineImage :: (Word8 -> Word8 -> Word8 -> Word8) -> RGBDW -> GreyDW
combineImage f arr = let sh@(Z :. y :. x :. _) = extent arr in fromFunction (Z :. y :. x) g
     where
        g (Z :. i :. j) = f r g b
            where
                r = arr ! (Z :. i :. j :. 0)
                g = arr ! (Z :. i :. j :. 1)
                b = arr ! (Z :. i :. j :. 2)
{-# INLINE combineImage #-}

rgbToGrey :: Word8 -> Word8 -> Word8 -> Word8
rgbToGrey !r !g !b = 
  let 
    rd = fromIntegral r :: Double
    gd = fromIntegral g :: Double
    bd = fromIntegral b :: Double
  in ceiling $ 0.21 * rd + 0.71 * gd + 0.07 * bd
{-# INLINE rgbToGrey #-}
