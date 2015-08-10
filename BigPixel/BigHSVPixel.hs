{-# LANGUAGE BangPatterns
           , CPP
           , RecordWildCards
           , TypeFamilies
           , TypeOperators #-}

module BigPixel.BigHSVPixel (
      BigHSV, BigHSVPixel (..), BigHSVDelayed
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Data.Word
import Data.Int
import Foreign.Storable (Storable (..))
import Foreign.Ptr (castPtr, plusPtr)

import Vision.Image.Class (Pixel (..))
import Vision.Image.Interpolate (Interpolable (..))
import Vision.Image.Type (Manifest, Delayed)

data BigHSVPixel = BigHSVPixel {
      hsvHue   :: {-# UNPACK #-} !Int16, hsvSat :: {-# UNPACK #-} !Int16
    , hsvValue :: {-# UNPACK #-} !Int16
    } deriving (Eq, Show)

-- | BigHSV uses Int16 instead of Word8 to be bigger + have negative values
-- | Allows for easier averaging, image arithmetic
--
-- | 24 bits (3 * 8 bits) HSV image.
--
-- The Hue value is in [0..179], Saturation in [0..255] and Value in [0..255].
--
-- This image type is more respectful to human eye perception of colors and can
-- be converted (using 'convert') from 'RGB' images.
--
-- Uses <http://en.wikipedia.org/wiki/HSL_and_HSV> equations to convert from and
-- to RGB.
type BigHSV = Manifest BigHSVPixel

type BigHSVDelayed = Delayed BigHSVPixel

instance Storable BigHSVPixel where
    sizeOf _ = 3 * sizeOf (undefined :: Int16)
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: Int16)
    {-# INLINE alignment #-}

    peek !ptr =
        let !ptr' = castPtr ptr
        in BigHSVPixel <$> peek ptr'               <*> peek (ptr' `plusPtr` 1)
                    <*> peek (ptr' `plusPtr` 2)
    {-# INLINE peek #-}

    poke !ptr BigHSVPixel { .. } =
        let !ptr' = castPtr ptr
        in poke ptr'               hsvHue   >>
           poke (ptr' `plusPtr` 1) hsvSat   >>
           poke (ptr' `plusPtr` 2) hsvValue
    {-# INLINE poke #-}

instance Pixel BigHSVPixel where
    type PixelChannel BigHSVPixel = Int16

    pixNChannels _ = 3
    {-# INLINE pixNChannels #-}

    pixIndex !(BigHSVPixel h _ _) 0 = h
    pixIndex !(BigHSVPixel _ s _) 1 = s
    pixIndex !(BigHSVPixel _ _ v) _ = v
    {-# INLINE pixIndex #-}

instance Interpolable BigHSVPixel where
    interpol f a b =
        let BigHSVPixel aHue aSat aVal = a
            BigHSVPixel bHue bSat bVal = b
        in BigHSVPixel {
              hsvHue   = f aHue bHue, hsvSat = f aSat bSat
            , hsvValue = f aVal bVal
            }
    {-# INLINE interpol #-}