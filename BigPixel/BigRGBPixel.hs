{-# LANGUAGE BangPatterns
           , CPP
           , RecordWildCards
           , TypeFamilies
           , TypeOperators #-}


module BigPixel.BigRGBPixel (
    BigRGBPixel (..), BigRGB, BigRGBDelayed
 ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Vision.Image.RGB.Type

import Data.Int
import Foreign.Storable (Storable (..))
import Foreign.Ptr (castPtr, plusPtr)

import Vision.Image.Class (Pixel (..))
import Vision.Image.Interpolate (Interpolable (..))
import Vision.Image.Type (Manifest, Delayed)


data BigRGBPixel = BigRGBPixel {
      rgbRed   :: {-# UNPACK #-} !Int16, rgbGreen :: {-# UNPACK #-} !Int16
    , rgbBlue  :: {-# UNPACK #-} !Int16
    } deriving (Eq, Show)

type BigRGB = Manifest BigRGBPixel

type BigRGBDelayed = Delayed BigRGBPixel

instance Storable BigRGBPixel where
    sizeOf _ = 3 * sizeOf (undefined :: Int16)
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: Int16)
    {-# INLINE alignment #-}

    peek !ptr =
        let !ptr' = castPtr ptr
        in BigRGBPixel <$> peek ptr'               <*> peek (ptr' `plusPtr` 1)
                    <*> peek (ptr' `plusPtr` 2)
    {-# INLINE peek #-}

    poke !ptr BigRGBPixel { .. } =
        let !ptr' = castPtr ptr
        in poke ptr'               rgbRed   >>
           poke (ptr' `plusPtr` 1) rgbGreen >>
           poke (ptr' `plusPtr` 2) rgbBlue
    {-# INLINE poke #-}

instance Pixel BigRGBPixel where
    type PixelChannel BigRGBPixel = Int16

    pixNChannels _ = 3
    {-# INLINE pixNChannels #-}

    pixIndex !(BigRGBPixel r _ _) 0 = r
    pixIndex !(BigRGBPixel _ g _) 1 = g
    pixIndex !(BigRGBPixel _ _ b) _ = b
    {-# INLINE pixIndex #-}

instance Interpolable BigRGBPixel where
    interpol f a b =
        let BigRGBPixel aRed aGreen aBlue = a
            BigRGBPixel bRed bGreen bBlue = b
        in BigRGBPixel {
              rgbRed  = f aRed  bRed, rgbGreen = f aGreen bGreen
            , rgbBlue = f aBlue bBlue
            }
    {-# INLINE interpol #-}
