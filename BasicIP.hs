{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies #-}

module BasicIP
(       toBlackAndWhite
    ,   brighten
    ,   contrast
) where

import Vision.Image
import Prelude hiding (map)
import PixelMath
import Data.Word
import Data.Int
import BigPixel.BigRGBPixel
import BigPixel.PixelSize 

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

-- Found in Friday examples. Copied here.
toBlackAndWhite :: (Image i, Convertible i Grey) => i -> Grey
toBlackAndWhite img =
    let grey = convert img :: Grey
    in map (\pix -> if pix > 127 then 255 else 0) grey