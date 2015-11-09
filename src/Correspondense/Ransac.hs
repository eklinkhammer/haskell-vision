{-# LANGUAGE PackageImports, BangPatterns, FlexibleContexts, AllowAmbiguousTypes  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module RobotVision.Correspondense.Ransac
(       test
) where

import Numeric.Ransac (ransac)
import RobotVision.FeatureDetection.Feature
import Data.Vector

--function :: Vector a -> Vector a
--function x = x

-- From Github:
-- | @ransac iter sampleSize agreePct fit residual goodFit pts@ draws
-- @iter@ samples of size @sampleSize@ from @pts@. The @fit@ function
-- is used to produce a model from each of these samples. The elements
-- of @pts@ whose residuals pass the @goodFit@ predicate with respect
-- to this model are identified as /inliers/, and used to update the
-- model. The model for which the size of the inliers set is at least
-- @agreePct@ percent of the entire data set and whose error over all
-- points is minimal among all sampled models is returned. If no
-- acceptable model is found (i.e. no model whose inliers were at
-- least @agreePct@ percent of the entire data set), 'Nothing' is
-- returned.
--ransac :: (V.Vector v a, V.Vector v d, Num d, Ord d) =>
--          Int -- maxIter
--          -> Int -- sampleSize
--          -> Float -- agreePct
--          -> (v a -> Maybe c) -- fit
--          -> (c -> a -> d) -- residual
--          -> (d -> Bool) -- goodFit
--          -> v a -- pts
--          -> IO (Maybe (c, v a))


-- we start with [Feature] and [DIM2] from both images

--
-- DIM2 -> 
-- (v a -> Maybe c)
-- subset of points -> model
-- c is the homography from solving the system defined by v a

myFitFunction :: (Vector v a) => v a -> v a -> Maybe c
myFitFunction secondImagePoints firstImagePoints = undefined
-- build system of equation, return x