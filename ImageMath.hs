{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies #-}

module ImageMath
(       toBlackAndWhite
    ,   brighten
    ,   contrast
    , testAddImage
    , testConstantImage
    , testAddChannelImage
    , bigSmall
) where

import Vision.Image
import Prelude hiding (map)
import PixelMath
import Data.Word
import Data.Int
import BigPixel.BigRGBPixel
import BigPixel.PixelSize 

bigSmall :: (Image i, FromFunction i, FromFunctionPixel i ~ ImagePixel i,
            PixelSize (ImagePixel i) (BigRGBPixel),
            FunctorImage i i) => i -> i
bigSmall img = map (\pix -> shrink $ expand pix) img

bigSmall' :: (Image i, FromFunction i, FromFunctionPixel i ~ ImagePixel i,
            PixelSize (ImagePixel i) (BigRGBPixel),
            FunctorImage i i) => i -> i
bigSmall' img = fromFunction (shape img) myFunction
    where 
        myFunction pt = shrink (expand ((!)img pt))

 --Brighten, using let and adding pixels
 --Specifically works for RGBPixels
--brighten :: (Image i, FromFunction i, FromFunctionPixel i ~ ImagePixel i,
--            PixelSize (ImagePixel i) (BigRGBPixel), -- we can expand and shrink the pixel
--            Convertible i RGB, -- we can convert to rgb
--            FunctorImage i i) => i -> i
--brighten img = fromFunction (shape img) myFunction
--    where
--        rgb = convert img :: RGB
--        myFunction pt =
--            let pixel = (expand . (!) rgb) pt
--            in shrink $ addPixel pixel pixel

--brighten :: (Image i, FromFunction i, FromFunctionPixel i ~ ImagePixel i,
--            PixelSize (ImagePixel i) (BigRGBPixel), -- we can expand and shrink the pixel
--            Convertible i RGB, -- we can convert to rgb
--            FunctorImage RGB i) => Int16 -> i -> i
--brighten x img = map (\pix -> shrink $ flip multPixel x (expand pix)) (convert img :: RGB)



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


toBlackAndWhite :: (Image i, Convertible i Grey) => i -> Grey
toBlackAndWhite img =
    let grey = convert img :: Grey
    in fromFunction (shape img) $ \pt ->
            if grey `index` pt > 127 then 255
                                     else 0

toBlackAndWhite' :: (Image i, Convertible i Grey) => i -> Grey
toBlackAndWhite' img =
    let grey = convert img :: Grey
    in map (\pix -> if pix > 127 then 255 else 0) grey


testAddImage :: (FromFunction i, Image i, FromFunctionPixel i ~ ImagePixel i,
                NumPixel (ImagePixel i) (ImagePixel i) (ImagePixel i)) => i -> i -> i
testAddImage a b = fromFunction (shape a) myFunction
    where
        myFunction pt = subPixel ((!)a pt) ((!)b pt)


testConstantImage :: (Num n, FromFunction i, Image i, FromFunctionPixel i ~ ImagePixel i, 
                    NumPixel (ImagePixel i) (n) (ImagePixel i)) => i -> n -> i
testConstantImage img x = fromFunction (shape img) myFunction
    where
        myFunction pt = addPixel ((!)img pt) x

testAddChannelImage :: (FromFunction i, Image i, FromFunctionPixel i ~ ImagePixel i, 
                        NumPixelChannel (ImagePixel i) (ImagePixel i) Word8 (ImagePixel i)) 
                        => i -> i -> Word8 -> i
testAddChannelImage img1 img2 channelNum = fromFunction (shape img1) myFunction
    where
        myFunction pt = addChannel ((!)img1 pt) ((!)img2 pt) (channelNum)

-- Identity
myIdentity :: (FromFunction i, Image i, FromFunctionPixel i ~ ImagePixel i) => i -> i
myIdentity img = fromFunction (shape img) myFunction
    where 
        myFunction pt = (!)img pt


