{-# LANGUAGE FunctionalDependencies
            , BangPatterns
            , FlexibleInstances #-}

module PixelMath (
      NumPixel (..)
    , NumPixelChannel (..)
) where

-- Module contains two classes: NumPixel and NumPixelChannel
-- Both classes allow for operations to be performed on a pixel, with either another pixel
--  or a constant factor. The difference is that NumPixel operates on a per pixel basis, and
--  NumPixelChannel operates on a single channel

-- The classes are defined for addition, subtraction, multiplication, and division with Word8
--  and Int16 respectively, and for RGB, Grey, HSV, and their Big Pixel equivelents.

-- This class does not convert between representations, and does not do any clamping or logic
--  with the operation that would prevent overflow. 

-- This is a major issue with the Word8 representations, but since that is the default value for
--  image storage, it is left as is. There are no plans to implement any logical operations to
--  deal with overflow in an intelligent matter (for example, the addition of 2 numbers will 
--  be the max of 255 in the event of overflow, else the sum) because the overhead of this check
--  necessary to account for all cases (check that a + b is not less than both a and b) triples 
--  the number of operations. An Int16 is only double the memory.

-- Note that there is also no support for floating point multiplication or division. There are plans
--  for a DoublePixel class family. In the meantime, you can express floating point multiplication as
--  a combination multiplication and division. ( * 1.2 = * 6 ->  / 5)
import Vision.Image.Class
import Vision.Image.Grey.Type (GreyPixel (..))
import Vision.Image.HSV.Type (HSVPixel (..))
import Vision.Image.RGBA.Type (RGBAPixel (..))
import Vision.Image.RGB.Type (RGBPixel (..))
import Data.Word
import Data.Int

import BigPixel.BigRGBPixel
import BigPixel.BigHSVPixel
import BigPixel.BigGreyPixel


class NumPixel a b c | a b -> c where
    addPixel :: a -> b -> c
    subPixel :: a -> b -> c
    multPixel :: a -> b -> c
    divPixel :: a -> b -> c

-- Pixel Combinations
instance NumPixel RGBPixel RGBPixel RGBPixel where
    addPixel !(RGBPixel r1 g1 b1) !(RGBPixel r2 g2 b2) = RGBPixel (r1+r2) (g1+g2) (b1+b2)
    subPixel !(RGBPixel r1 g1 b1) !(RGBPixel r2 g2 b2) = RGBPixel (r1-r2) (g1-g2) (b1-b2)
    multPixel !(RGBPixel r1 g1 b1) !(RGBPixel r2 g2 b2) = RGBPixel (r1*r2) (g1*g2) (b1*b2)
    divPixel !(RGBPixel r1 g1 b1) !(RGBPixel r2 g2 b2) 
        = RGBPixel (safeDiv (r1 :: Word8) (r2 :: Word8)) (safeDiv (g1 :: Word8) (g2 :: Word8)) (safeDiv (b1 :: Word8) (b2 :: Word8))

instance NumPixel GreyPixel GreyPixel GreyPixel where
    addPixel !(GreyPixel val1) !(GreyPixel val2) = GreyPixel (val1+val2)
    subPixel !(GreyPixel val1) !(GreyPixel val2) = GreyPixel (val1-val2)
    multPixel !(GreyPixel val1) !(GreyPixel val2) = GreyPixel (val1*val2)
    divPixel !(GreyPixel val1) !(GreyPixel val2) = GreyPixel (safeDiv (val1 :: Word8) (val2 :: Word8))

instance NumPixel HSVPixel HSVPixel HSVPixel where
    addPixel !(HSVPixel r1 g1 b1) !(HSVPixel r2 g2 b2) = HSVPixel (r1+r2) (g1+g2) (b1+b2)
    subPixel !(HSVPixel r1 g1 b1) !(HSVPixel r2 g2 b2) = HSVPixel (r1-r2) (g1-g2) (b1-b2)
    multPixel !(HSVPixel r1 g1 b1) !(HSVPixel r2 g2 b2) = HSVPixel (r1*r2) (g1*g2) (b1*b2)
    divPixel !(HSVPixel r1 g1 b1) !(HSVPixel r2 g2 b2) 
        = HSVPixel (safeDiv (r1 :: Word8) (r2 :: Word8)) (safeDiv (g1 :: Word8) (g2 :: Word8)) (safeDiv (b1 :: Word8) (b2 :: Word8))

-- Need to define a RGBA instance for completeness at some point

--- Big Pixel Combinations
instance NumPixel BigRGBPixel BigRGBPixel BigRGBPixel where
    addPixel !(BigRGBPixel r1 g1 b1) !(BigRGBPixel r2 g2 b2) = BigRGBPixel (r1+r2) (g1+g2) (b1+b2)
    subPixel !(BigRGBPixel r1 g1 b1) !(BigRGBPixel r2 g2 b2) = BigRGBPixel (r1-r2) (g1-g2) (b1-b2)
    multPixel !(BigRGBPixel r1 g1 b1) !(BigRGBPixel r2 g2 b2) = BigRGBPixel (r1*r2) (g1*g2) (b1*b2)
    divPixel !(BigRGBPixel r1 g1 b1) !(BigRGBPixel r2 g2 b2) 
        = BigRGBPixel (safeDiv (r1 :: Int16) (r2 :: Int16)) (safeDiv (g1 :: Int16) (g2 :: Int16)) (safeDiv (b1 :: Int16) (b2 :: Int16))


instance NumPixel BigGreyPixel BigGreyPixel BigGreyPixel where
    addPixel !(BigGreyPixel val1) !(BigGreyPixel val2) = BigGreyPixel (val1+val2)
    subPixel !(BigGreyPixel val1) !(BigGreyPixel val2) = BigGreyPixel (val1-val2)
    multPixel !(BigGreyPixel val1) !(BigGreyPixel val2) = BigGreyPixel (val1*val2)
    divPixel !(BigGreyPixel val1) !(BigGreyPixel val2) = BigGreyPixel (safeDiv (val1 :: Int16) (val2 :: Int16))

instance NumPixel BigHSVPixel BigHSVPixel BigHSVPixel where
    addPixel !(BigHSVPixel r1 g1 b1) !(BigHSVPixel r2 g2 b2) = BigHSVPixel (r1+r2) (g1+g2) (b1+b2)
    subPixel !(BigHSVPixel r1 g1 b1) !(BigHSVPixel r2 g2 b2) = BigHSVPixel (r1-r2) (g1-g2) (b1-b2)
    multPixel !(BigHSVPixel r1 g1 b1) !(BigHSVPixel r2 g2 b2) = BigHSVPixel (r1*r2) (g1*g2) (b1*b2)
    divPixel !(BigHSVPixel r1 g1 b1) !(BigHSVPixel r2 g2 b2) 
        = BigHSVPixel (safeDiv (r1 :: Int16) (r2 :: Int16)) (safeDiv (g1 :: Int16) (g2 :: Int16)) (safeDiv (b1 :: Int16) (b2 :: Int16))


-- Pixel operations, on all channels, with a constant value
-- Most useful for division and multiplication.
-- BigPixel's only accept Int16 values, and all other ImagePixel take Word8
-- Pixel Combinations
instance NumPixel Word8 RGBPixel RGBPixel where
    addPixel n !(RGBPixel r g b) = RGBPixel (r+n) (g+n) (b+n)
    subPixel n !(RGBPixel r g b) = RGBPixel (r-n) (g-n) (b-n)
    multPixel n !(RGBPixel r g b) = RGBPixel (r*n) (g*n) (b*n)
    divPixel n !(RGBPixel r g b)
        = RGBPixel (safeDiv (r :: Word8) (n)) (safeDiv (g :: Word8) (n)) (safeDiv (b :: Word8) (n))

instance NumPixel Word8 GreyPixel GreyPixel where
    addPixel n !(GreyPixel val) = GreyPixel (val+n)
    subPixel n !(GreyPixel val) = GreyPixel (val-n)
    multPixel n !(GreyPixel val) = GreyPixel (val*n)
    divPixel n !(GreyPixel val) = GreyPixel (safeDiv (val :: Word8) (n))

instance NumPixel Word8 HSVPixel HSVPixel where
    addPixel n !(HSVPixel r g b) = HSVPixel (r+n) (g+n) (b+n)
    subPixel n !(HSVPixel r g b) = HSVPixel (r-n) (g-n) (b-n)
    multPixel n !(HSVPixel r g b) = HSVPixel (r*n) (g*n) (b*n)
    divPixel n !(HSVPixel r g b) 
        = HSVPixel (safeDiv (r :: Word8) (n)) (safeDiv (g :: Word8) (n)) (safeDiv (b :: Word8) (n))

-- Need to define a RGBA instance for completeness at some point

--- Big Pixel Combinations
instance NumPixel Int16 BigRGBPixel BigRGBPixel where
    addPixel n !(BigRGBPixel r g b) = BigRGBPixel (r+n) (g+n) (b+n)
    subPixel n !(BigRGBPixel r g b) = BigRGBPixel (r-n) (g-n) (b-n)
    multPixel n !(BigRGBPixel r g b) = BigRGBPixel (r*n) (g*n) (b*n)
    divPixel n !(BigRGBPixel r g b) 
        = BigRGBPixel (safeDiv (r :: Int16) (n)) (safeDiv (g :: Int16) (n)) (safeDiv (b :: Int16) (n))


instance NumPixel Int16 BigGreyPixel BigGreyPixel where
    addPixel n !(BigGreyPixel val) = BigGreyPixel (val+n)
    subPixel n !(BigGreyPixel val) = BigGreyPixel (val-n)
    multPixel n !(BigGreyPixel val) = BigGreyPixel (val*n)
    divPixel n !(BigGreyPixel val) = BigGreyPixel (safeDiv (val :: Int16) (n))

instance NumPixel Int16 BigHSVPixel BigHSVPixel where
    addPixel n !(BigHSVPixel r g b) = BigHSVPixel (r+n) (g+n) (b+n)
    subPixel n !(BigHSVPixel r g b) = BigHSVPixel (r-n) (g-n) (b-n)
    multPixel n !(BigHSVPixel r g b) = BigHSVPixel (r*n) (g*n) (b*n)
    divPixel n !(BigHSVPixel r g b) 
        = BigHSVPixel (safeDiv (r :: Int16) (n)) (safeDiv (g :: Int16) (n)) (safeDiv (b :: Int16) (n))

class NumPixelChannel a b c d | a b c -> d where
    addChannel :: a -> b -> c -> d
    subChannel :: a -> b -> c -> d
    multChannel :: a -> b -> c -> d
    divChannel :: a -> b -> c -> d

-- Regular Pixel by Channel
instance NumPixelChannel RGBPixel RGBPixel Word8 RGBPixel where
    addChannel !(RGBPixel r1 g1 b1) !(RGBPixel r2 g2 b2) n
        | n == 3 = RGBPixel r1 g1 (b1+b2)
        | n == 2 = RGBPixel r1 (g1+g2) b1
        | n == 1 = RGBPixel (r1+r2) g1 b1
        | otherwise = RGBPixel r1 g1 b1
    subChannel !(RGBPixel r1 g1 b1) !(RGBPixel r2 g2 b2) n
        | n == 3 = RGBPixel r1 g1 (b1-b2)
        | n == 2 = RGBPixel r1 (g1-g2) b1
        | n == 1 = RGBPixel (r1-r2) g1 b1
        | otherwise = RGBPixel r1 g1 b1
    multChannel !(RGBPixel r1 g1 b1) !(RGBPixel r2 g2 b2) n
        | n == 3 = RGBPixel r1 g1 (b1*b2)
        | n == 2 = RGBPixel r1 (g1*g2) b1
        | n == 1 = RGBPixel (r1*r2) g1 b1
        | otherwise = RGBPixel r1 g1 b1
    divChannel !(RGBPixel r1 g1 b1) !(RGBPixel r2 g2 b2) n
        | n == 3 = RGBPixel r1 g1 (safeDiv (b1 :: Word8) (b2 :: Word8))
        | n == 2 = RGBPixel r1 (safeDiv (g1 :: Word8) (g2 :: Word8)) b1
        | n == 1 = RGBPixel (safeDiv (r1 :: Word8) (r2 :: Word8)) g1 b1
        | otherwise = RGBPixel r1 g1 b1

instance NumPixelChannel HSVPixel HSVPixel Word8 HSVPixel where
    addChannel !(HSVPixel h1 s1 v1) !(HSVPixel h2 s2 v2) n
        | n == 3 = HSVPixel h1 s1 (v1+v2)
        | n == 2 = HSVPixel h1 (s1+s2) v1
        | n == 1 = HSVPixel (h1+h2) s1 v1
        | otherwise = HSVPixel h1 s1 v1
    subChannel !(HSVPixel h1 s1 v1) !(HSVPixel h2 s2 v2) n
        | n == 3 = HSVPixel h1 s1 (v1-v2)
        | n == 2 = HSVPixel h1 (s1-s2) v1
        | n == 1 = HSVPixel (h1-h2) s1 v1
        | otherwise = HSVPixel h1 s1 v1
    multChannel !(HSVPixel h1 s1 v1) !(HSVPixel h2 s2 v2) n
        | n == 3 = HSVPixel h1 s1 (v1*v2)
        | n == 2 = HSVPixel h1 (s1*s2) v1
        | n == 1 = HSVPixel (h1*h2) s1 v1
        | otherwise = HSVPixel h1 s1 v1
    divChannel !(HSVPixel h1 s1 v1) !(HSVPixel h2 s2 v2) n
        | n == 3 = HSVPixel h1 s1 (safeDiv (v1 :: Word8) (v2 :: Word8))
        | n == 2 = HSVPixel h1 (safeDiv (s1 :: Word8) (s2 :: Word8)) v1
        | n == 1 = HSVPixel (safeDiv (h1 :: Word8) (h2 :: Word8)) s1 v1
        | otherwise = HSVPixel h1 s1 v1

-- BigPixel by channel
instance NumPixelChannel BigRGBPixel BigRGBPixel Word8 BigRGBPixel where
    addChannel !(BigRGBPixel r1 g1 b1) !(BigRGBPixel r2 g2 b2) n
        | n == 3 = BigRGBPixel r1 g1 (b1+b2)
        | n == 2 = BigRGBPixel r1 (g1+g2) b1
        | n == 1 = BigRGBPixel (r1+r2) g1 b1
        | otherwise = BigRGBPixel r1 g1 b1
    subChannel !(BigRGBPixel r1 g1 b1) !(BigRGBPixel r2 g2 b2) n
        | n == 3 = BigRGBPixel r1 g1 (b1-b2)
        | n == 2 = BigRGBPixel r1 (g1-g2) b1
        | n == 1 = BigRGBPixel (r1-r2) g1 b1
        | otherwise = BigRGBPixel r1 g1 b1
    multChannel !(BigRGBPixel r1 g1 b1) !(BigRGBPixel r2 g2 b2) n
        | n == 3 = BigRGBPixel r1 g1 (b1*b2)
        | n == 2 = BigRGBPixel r1 (g1*g2) b1
        | n == 1 = BigRGBPixel (r1*r2) g1 b1
        | otherwise = BigRGBPixel r1 g1 b1
    divChannel !(BigRGBPixel r1 g1 b1) !(BigRGBPixel r2 g2 b2) n
        | n == 3 = BigRGBPixel r1 g1 (safeDiv (b1 :: Int16) (b2 :: Int16))
        | n == 2 = BigRGBPixel r1 (safeDiv (g1 :: Int16) (g2 :: Int16)) b1
        | n == 1 = BigRGBPixel (safeDiv (r1 :: Int16) (r2 :: Int16)) g1 b1
        | otherwise = BigRGBPixel r1 g1 b1

instance NumPixelChannel BigHSVPixel BigHSVPixel Word8 BigHSVPixel where
    addChannel !(BigHSVPixel h1 s1 v1) !(BigHSVPixel h2 s2 v2) n
        | n == 3 = BigHSVPixel h1 s1 (v1+v2)
        | n == 2 = BigHSVPixel h1 (s1+s2) v1
        | n == 1 = BigHSVPixel (h1+h2) s1 v1
        | otherwise = BigHSVPixel h1 s1 v1
    subChannel !(BigHSVPixel h1 s1 v1) !(BigHSVPixel h2 s2 v2) n
        | n == 3 = BigHSVPixel h1 s1 (v1-v2)
        | n == 2 = BigHSVPixel h1 (s1-s2) v1
        | n == 1 = BigHSVPixel (h1-h2) s1 v1
        | otherwise = BigHSVPixel h1 s1 v1
    multChannel !(BigHSVPixel h1 s1 v1) !(BigHSVPixel h2 s2 v2) n
        | n == 3 = BigHSVPixel h1 s1 (v1*v2)
        | n == 2 = BigHSVPixel h1 (s1*s2) v1
        | n == 1 = BigHSVPixel (h1*h2) s1 v1
        | otherwise = BigHSVPixel h1 s1 v1
    divChannel !(BigHSVPixel h1 s1 v1) !(BigHSVPixel h2 s2 v2) n
        | n == 3 = BigHSVPixel h1 s1 (safeDiv (v1 :: Int16) (v2 :: Int16))
        | n == 2 = BigHSVPixel h1 (safeDiv (s1 :: Int16) (s2 :: Int16)) v1
        | n == 1 = BigHSVPixel (safeDiv (h1 :: Int16) (h2 :: Int16)) s1 v1
        | otherwise = BigHSVPixel h1 s1 v1

-- Single channel, regular pixel, constant
instance NumPixelChannel Word8 Word8 RGBPixel RGBPixel where
    addChannel n x !(RGBPixel r1 g1 b1)
        | n == 3 = RGBPixel r1 g1 (b1+x)
        | n == 2 = RGBPixel r1 (g1+x) b1
        | n == 1 = RGBPixel (r1+x) g1 b1
        | otherwise = RGBPixel r1 g1 b1
    subChannel n x !(RGBPixel r1 g1 b1)
        | n == 3 = RGBPixel r1 g1 (b1-x)
        | n == 2 = RGBPixel r1 (g1-x) b1
        | n == 1 = RGBPixel (r1-x) g1 b1
        | otherwise = RGBPixel r1 g1 b1
    multChannel n x !(RGBPixel r1 g1 b1)
        | n == 3 = RGBPixel r1 g1 (b1*x)
        | n == 2 = RGBPixel r1 (g1*x) b1
        | n == 1 = RGBPixel (r1*x) g1 b1
        | otherwise = RGBPixel r1 g1 b1
    divChannel n x !(RGBPixel r1 g1 b1)
        | n == 3 = RGBPixel r1 g1 (safeDiv (b1 :: Word8) (x :: Word8))
        | n == 2 = RGBPixel r1 (safeDiv (g1 :: Word8) (x :: Word8)) b1
        | n == 1 = RGBPixel (safeDiv (r1 :: Word8) (x :: Word8)) g1 b1
        | otherwise = RGBPixel r1 g1 b1

instance NumPixelChannel Word8 Word8 HSVPixel HSVPixel where
    addChannel n x !(HSVPixel h1 s1 v1)
        | n == 3 = HSVPixel h1 s1 (v1+x)
        | n == 2 = HSVPixel h1 (s1+x) v1
        | n == 1 = HSVPixel (h1+x) s1 v1
        | otherwise = HSVPixel h1 s1 v1
    subChannel n x !(HSVPixel h1 s1 v1)
        | n == 3 = HSVPixel h1 s1 (v1-x)
        | n == 2 = HSVPixel h1 (s1-x) v1
        | n == 1 = HSVPixel (h1-x) s1 v1
        | otherwise = HSVPixel h1 s1 v1
    multChannel n x !(HSVPixel h1 s1 v1)
        | n == 3 = HSVPixel h1 s1 (v1*x)
        | n == 2 = HSVPixel h1 (s1*x) v1
        | n == 1 = HSVPixel (h1*x) s1 v1
        | otherwise = HSVPixel h1 s1 v1
    divChannel n x !(HSVPixel h1 s1 v1)
        | n == 3 = HSVPixel h1 s1 (safeDiv (v1 :: Word8) (x :: Word8))
        | n == 2 = HSVPixel h1 (safeDiv (s1 :: Word8) (x :: Word8)) v1
        | n == 1 = HSVPixel (safeDiv (h1 :: Word8) (x :: Word8)) s1 v1
        | otherwise = HSVPixel h1 s1 v1


-- Single channel, BigPixel, constant
instance NumPixelChannel Word8 Int16 BigRGBPixel BigRGBPixel where
    addChannel n x !(BigRGBPixel r1 g1 b1)
        | n == 3 = BigRGBPixel r1 g1 (b1+x)
        | n == 2 = BigRGBPixel r1 (g1+x) b1
        | n == 1 = BigRGBPixel (r1+x) g1 b1
        | otherwise = BigRGBPixel r1 g1 b1
    subChannel n x !(BigRGBPixel r1 g1 b1)
        | n == 3 = BigRGBPixel r1 g1 (b1-x)
        | n == 2 = BigRGBPixel r1 (g1-x) b1
        | n == 1 = BigRGBPixel (r1-x) g1 b1
        | otherwise = BigRGBPixel r1 g1 b1
    multChannel n x !(BigRGBPixel r1 g1 b1)
        | n == 3 = BigRGBPixel r1 g1 (b1*x)
        | n == 2 = BigRGBPixel r1 (g1*x) b1
        | n == 1 = BigRGBPixel (r1*x) g1 b1
        | otherwise = BigRGBPixel r1 g1 b1
    divChannel n x !(BigRGBPixel r1 g1 b1)
        | n == 3 = BigRGBPixel r1 g1 (safeDiv (b1 :: Int16) (x :: Int16))
        | n == 2 = BigRGBPixel r1 (safeDiv (g1 :: Int16) (x :: Int16)) b1
        | n == 1 = BigRGBPixel (safeDiv (r1 :: Int16) (x :: Int16)) g1 b1
        | otherwise = BigRGBPixel r1 g1 b1

instance NumPixelChannel Word8 Int16 BigHSVPixel BigHSVPixel where
    addChannel n x !(BigHSVPixel h1 s1 v1)
        | n == 3 = BigHSVPixel h1 s1 (v1+x)
        | n == 2 = BigHSVPixel h1 (s1+x) v1
        | n == 1 = BigHSVPixel (h1+x) s1 v1
        | otherwise = BigHSVPixel h1 s1 v1
    subChannel n x !(BigHSVPixel h1 s1 v1)
        | n == 3 = BigHSVPixel h1 s1 (v1-x)
        | n == 2 = BigHSVPixel h1 (s1-x) v1
        | n == 1 = BigHSVPixel (h1-x) s1 v1
        | otherwise = BigHSVPixel h1 s1 v1
    multChannel n x !(BigHSVPixel h1 s1 v1)
        | n == 3 = BigHSVPixel h1 s1 (v1*x)
        | n == 2 = BigHSVPixel h1 (s1*x) v1
        | n == 1 = BigHSVPixel (h1*x) s1 v1
        | otherwise = BigHSVPixel h1 s1 v1
    divChannel n x !(BigHSVPixel h1 s1 v1)
        | n == 3 = BigHSVPixel h1 s1 (safeDiv (v1 :: Int16) (x :: Int16))
        | n == 2 = BigHSVPixel h1 (safeDiv (s1 :: Int16) (x :: Int16)) v1
        | n == 1 = BigHSVPixel (safeDiv (h1 :: Int16) (x :: Int16)) s1 v1
        | otherwise = BigHSVPixel h1 s1 v1
-- functions
safeDiv :: (Integral a) => a -> a -> a
safeDiv x y 
    | y == 0 = 255
    | otherwise = div x y
