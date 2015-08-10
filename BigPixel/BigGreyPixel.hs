{-# LANGUAGE BangPatterns
           , GeneralizedNewtypeDeriving
           , TypeFamilies #-}

module BigPixel.BigGreyPixel (
      BigGrey, BigGreyPixel (..), BigGreyDelayed
    ) where

import Data.Bits
import Data.Int
import Data.Word
import Foreign.Storable (Storable)

import Vision.Image.Class (Pixel (..))
import Vision.Image.Interpolate (Interpolable (..))
import Vision.Image.Type (Manifest, Delayed)

newtype BigGreyPixel = BigGreyPixel Int16
    deriving (Bits, Bounded, Enum, Eq, Integral, Num, Ord, Real, Read, Show
            , Storable)

type BigGrey = Manifest BigGreyPixel

type BigGreyDelayed = Delayed BigGreyPixel

instance Pixel BigGreyPixel where
    type PixelChannel BigGreyPixel = Int16

    pixNChannels _ = 1
    {-# INLINE pixNChannels #-}

    pixIndex !(BigGreyPixel v) _ = v
    {-# INLINE pixIndex #-}

instance Interpolable BigGreyPixel where
    interpol f (BigGreyPixel a) (BigGreyPixel b) = BigGreyPixel $ f a b
    {-# INLINE interpol #-}