{-# LANGUAGE BangPatterns
           , CPP
           , RecordWildCards
           , TypeFamilies
           , TypeOperators #-}

module BigPixel.BigRGBAPixel (
      BigRGBA, BigRGBAPixel (..), BigRGBADelayed
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Data.Word
import Foreign.Storable (Storable (..))
import Foreign.Ptr (castPtr, plusPtr)

import Vision.Image.Class (Pixel (..))
import Vision.Image.Interpolate (Interpolable (..))
import Vision.Image.Type (Manifest, Delayed)

import Data.Int

data BigRGBAPixel = BigRGBAPixel {
      rgbaRed   :: {-# UNPACK #-} !Int16, rgbaGreen :: {-# UNPACK #-} !Int16
    , rgbaBlue  :: {-# UNPACK #-} !Int16, rgbaAlpha :: {-# UNPACK #-} !Int16
    } deriving (Eq, Show)

type BigRGBA = Manifest BigRGBAPixel

type BigRGBADelayed = Delayed BigRGBAPixel

instance Storable BigRGBAPixel where
    sizeOf _ = 4 * sizeOf (undefined :: Int16)
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: Int16)
    {-# INLINE alignment #-}

    peek !ptr =
        let !ptr' = castPtr ptr
        in BigRGBAPixel <$> peek ptr'               <*> peek (ptr' `plusPtr` 1)
                     <*> peek (ptr' `plusPtr` 2) <*> peek (ptr' `plusPtr` 3)
    {-# INLINE peek #-}

    poke !ptr BigRGBAPixel { .. } =
        let !ptr' = castPtr ptr
        in poke ptr'               rgbaRed   >>
           poke (ptr' `plusPtr` 1) rgbaGreen >>
           poke (ptr' `plusPtr` 2) rgbaBlue  >>
           poke (ptr' `plusPtr` 3) rgbaAlpha
    {-# INLINE poke #-}

instance Pixel BigRGBAPixel where
    type PixelChannel BigRGBAPixel    = Int16

    pixNChannels _ = 4
    {-# INLINE pixNChannels #-}

    pixIndex !(BigRGBAPixel r _ _ _) 0 = r
    pixIndex !(BigRGBAPixel _ g _ _) 1 = g
    pixIndex !(BigRGBAPixel _ _ b _) 2 = b
    pixIndex !(BigRGBAPixel _ _ _ a) _ = a
    {-# INLINE pixIndex #-}

instance Interpolable BigRGBAPixel where
    interpol f a b =
        let BigRGBAPixel aRed aGreen aBlue aAlpha = a
            BigRGBAPixel bRed bGreen bBlue bAlpha = b
        in BigRGBAPixel {
              rgbaRed  = f aRed  bRed,  rgbaGreen = f aGreen bGreen
            , rgbaBlue = f aBlue bBlue, rgbaAlpha = f aAlpha bAlpha
            }
    {-# INLINE interpol #-}