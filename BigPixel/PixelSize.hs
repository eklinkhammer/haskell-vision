{-# LANGUAGE BangPatterns
           , FunctionalDependencies
           , CPP
           , RecordWildCards
           , TypeFamilies
           , TypeOperators #-}

module BigPixel.PixelSize (
      PixelSize (..)
    ) where

import Vision.Image.Class
import Vision.Image.Grey.Type (GreyPixel (..))
import Vision.Image.HSV.Type (HSVPixel (..))
import Vision.Image.RGBA.Type (RGBAPixel (..))
import Vision.Image.RGB.Type (RGBPixel (..))

import Data.Int
import Data.Word

import BigPixel.BigRGBPixel
import BigPixel.BigRGBAPixel
import BigPixel.BigGreyPixel
import BigPixel.BigHSVPixel

class PixelSize a b | a -> b where
  expand :: a -> b
  shrink :: b -> a

-- RGBA, RGB, and Gray values range from 0 to 255
-- H values are 0 - 179 (degrees), SV are 0 - 255
-- Since pixels are read and written in Word8, all clamps will assume those values, regardless of anything else. This means 
--  in practice clamping should be the last operation performed.
instance PixelSize RGBPixel BigRGBPixel where
    expand !(RGBPixel r g b) = BigRGBPixel (fromIntegral r :: Int16) (fromIntegral g :: Int16) (fromIntegral b :: Int16)
    shrink !(BigRGBPixel r g b) = RGBPixel (word8 . clamp 0 255 $ r) (word8 . clamp 0 255 $ g) (word8 . clamp 0 255 $ b)

instance PixelSize RGBAPixel BigRGBAPixel where
    expand !(RGBAPixel r g b a) = BigRGBAPixel (fromIntegral r :: Int16) (fromIntegral g :: Int16) (fromIntegral b :: Int16) (fromIntegral a :: Int16)
    shrink !(BigRGBAPixel r g b a) = RGBAPixel (word8 . clamp 0 255 $ r) (word8 . clamp 0 255 $ g) (word8 . clamp 0 255 $ b) (word8 . clamp 0 255 $ a)

instance PixelSize GreyPixel BigGreyPixel where
    expand !(GreyPixel g) = BigGreyPixel (fromIntegral g :: Int16)
    shrink !(BigGreyPixel g) = GreyPixel (word8 . clamp 0 255 $ g)

instance PixelSize HSVPixel BigHSVPixel where
    expand !(HSVPixel r g b) = BigHSVPixel (fromIntegral r :: Int16) (fromIntegral g :: Int16) (fromIntegral b :: Int16)
    shrink !(BigHSVPixel r g b) = HSVPixel (word8 . clamp 0 179 $ r) (word8 . clamp 0 255 $ g) (word8 . clamp 0 255 $ b)


-- functions
clamp :: (Ord n) => n -> n -> n -> n
clamp min max num
    | max < num = max
    | min > num = min
    | otherwise = num

word8 :: Integral a => a -> Word8
word8 = fromIntegral
