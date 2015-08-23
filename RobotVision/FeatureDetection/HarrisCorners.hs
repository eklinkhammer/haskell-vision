{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleInstances
            #-}

module RobotVision.FeatureDetection.HarrisCorners
(   brighten
) where

import RobotVision.ImageRep.Class
import Prelude hiding (map)
import Data.Array.Repa
import Data.Word

test = id

expand' :: (Source r e, Shape sh, 
            Expandable (Array r sh e) (Array D sh Int)) 
            => Array r sh e -> Array D sh Int
expand' = expand

shrink' :: (Shape sh,
            Shrinkable (Array D sh Int) (Array D sh Word8))
            => Array D sh Int -> Array D sh Word8
shrink' = shrink

brighten :: (Source r e,
    Shape sh, 
    Expandable (Array r sh e) (Array D sh Int),
    Shrinkable (Array D sh Int) (Array D sh e))
    => Array r sh e -> Array D sh e
brighten = shrink . map (2 *) . expand