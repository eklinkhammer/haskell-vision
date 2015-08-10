{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies #-}

module BasicIP
(       toBlackAndWhite
    ,   brighten
    ,   contrast
    ,   lumiChromi
    ,   lumiChromiToRGB
) where

import Vision.Image
import Prelude hiding (map)
import PixelMath
import Data.Word
import Data.Int
import BigPixel.BigRGBPixel
import BigPixel.PixelSize
import Vision.Primitive

-- Brightens the source image by a multiplicative constant factor
brighten :: (Image i, FromFunction i, FromFunctionPixel i ~ ImagePixel i,
            PixelSize (ImagePixel i) (BigRGBPixel),
            Convertible i RGB,
            FunctorImage RGB i) 
            => Int16 -- Factor to increase brightness by
            -> i -- src
            -> i -- output
brighten !x img = let rgb = convert img :: RGB
                  in map (shrink . (multPixel x) . expand) rgb

-- Increases the contrast of an image about a specified midpoint by a constant factor
contrast :: (Image i, FromFunction i, FromFunctionPixel i ~ ImagePixel i,
            PixelSize (ImagePixel i) BigRGBPixel,
            Convertible i RGB,
            FunctorImage RGB i)
            => Int16 -- contrast factor
            -> Int16 -- Midpoint
            -> i -- src
            -> i -- output
contrast !factor !mid img 
    = let rgb = convert img :: RGB
      in map (shrink . (addPixel mid) . (multPixel factor) . (subPixel mid) . expand) rgb

-- Returns a tuple of two images. The first is the luminance of the input,
--  the second is the chrominance. 
-- This doesn't really make sense, since Grey and RGB should both be DIM2
lumiChromi :: RGB -> (Grey,RGB)
lumiChromi img = (lumi, chromi)
    where
        lumi = convert img :: Grey
        lumiShape = shape lumi
        chromiShape = shape img
        chromi = fromFunction chromiShape chrom
        chrom pt = 
            let 
                rgbPixel = (!)img pt :: RGBPixel
                greyPixel = (!)lumi (fromLinearIndex lumiShape (div (toLinearIndex chromiShape pt) 3)) :: GreyPixel
            in divPixel rgbPixel greyPixel

-- Reverses the rgb -> lumi, chrom transformation
lumiChromiToRGB :: (Grey, RGB) -> RGB
lumiChromiToRGB (lumi, chrom) = rgb
    where
        lumiShape = shape lumi
        chromiShape = shape chrom
        rgb = fromFunction chromiShape construct
        construct pt = 
            let
                chromPixel = (!)chrom pt :: RGBPixel
                lumiPixel = (!)lumi (fromLinearIndex lumiShape (div (toLinearIndex chromiShape pt) 3)) :: GreyPixel
            in shrink $ multPixel (expand chromPixel) (expand lumiPixel)

-- Found in Friday examples. Copied here.
toBlackAndWhite :: (Image i, Convertible i Grey) => i -> Grey
toBlackAndWhite img =
    let grey = convert img :: Grey
    in map (\pix -> if pix > 127 then 255 else 0) grey