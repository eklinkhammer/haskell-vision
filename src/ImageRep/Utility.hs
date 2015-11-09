{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleInstances #-}

module ImageRep.Utility
(     fromChannels
    , toChannels
    , getNotBoundaryPoints
    , getImagePoints
    , onBoundary
) where

import Data.Array.Repa
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Data.Array.Repa.Repr.ForeignPtr          as F
import Data.Word
import Prelude as P
import Data.Convertible

test = id

toChannels :: (Source r e)
  => Array r DIM3 e -> (Array D DIM2 e, Array D DIM2 e, Array D DIM2 e)
toChannels img = (r,g,b)
  where
    r = fromFunction (Z :. height :. width) rf
    g = fromFunction (Z :. height :. width) gf
    b = fromFunction (Z :. height :. width) bf
    (Z :. height :. width :. _) = extent img
    rf (Z :. y :. x) = img ! (Z :. y :. x :. 0)
    gf (Z :. y :. x) = img ! (Z :. y :. x :. 1)
    bf (Z :. y :. x) = img ! (Z :. y :. x :. 2)
{-# INLINE toChannels #-}

fromChannels :: (Source r e)
  => (Array r DIM2 e, Array r DIM2 e, Array r DIM2 e) -> Array D DIM3 e
fromChannels (r,g,b) =
  let (Z :. width :. height) = extent r
  in fromFunction (Z :. width :. height :. 3) f
    where
      f (Z :. y :. x :. z) = case z of
                              0 -> r ! (Z :. y :. x)
                              1 -> g ! (Z :. y :. x)
                              2 -> b ! (Z :. y :. x)
{-# INLINE fromChannels #-}

getImagePoints :: (Source r e) => Array r DIM2 e -> [DIM2]
getImagePoints img = let (Z :. y :. x) = extent img in P.map tupleToPoint $ permuteList [0 .. (y-1)] [0 .. (x-1)]

tupleList :: [a] -> b -> [(a,b)]
tupleList [] _ = []
tupleList (x:xs) y = (x,y) : (tupleList xs y)

permuteList :: [a] -> [b] -> [(a,b)]
permuteList _ [] = []
permuteList xs (y:ys) = tupleList xs y P.++ permuteList xs ys

tupleToPoint :: (Int, Int) -> DIM2
tupleToPoint (x,y) = Z :. x :. y

getNotBoundaryPoints :: (Source r e) => Array r DIM2 e -> [DIM2]
getNotBoundaryPoints img = notBoundaryPoints img $ getImagePoints img

notBoundaryPoints :: (Source r e) => Array r DIM2 e -> [DIM2] -> [DIM2]
notBoundaryPoints img pts =
  let (Z :. w :. h) = extent img in filter (not . (onBoundary w h)) pts

onTop :: Int -> DIM2 -> Bool
onTop height (Z :. y :. _) = y == (height - 1)

onBot :: DIM2 -> Bool
onBot (Z :. y :. _) = y == 0

onLeft :: DIM2 -> Bool
onLeft (Z :. _ :. x) = x == 0

onRight :: Int -> DIM2 -> Bool
onRight width (Z :. _ :. x) = x == (width - 1)

onBoundary :: Int -> Int -> DIM2 -> Bool
onBoundary height width pt = (onTop height pt) || (onBot pt) || (onLeft pt) || (onRight width pt)
