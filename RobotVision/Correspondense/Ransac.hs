{-# LANGUAGE PackageImports, BangPatterns, FlexibleContexts, AllowAmbiguousTypes  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module RobotVision.Correspondense.Ransac
(       test
) where

import Numeric.Ransac (ransac)
import RobotVision.FeatureDetection.Feature
--import Data.Vector

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

--myFitFunction :: (V.Vector v a) => v a -> v a -> Maybe c
--myFitFunction secondImagePoints firstImagePoints = -- build system of equation, return x

--buildFitFunction :: (V.Vector v a) => v a -> (v a -> Maybe c) -- c :: Matrix?
--buildFitFunction secondImagePoints = myFitFunction secondImagePoints
  --Ax = b
  --| x y 1 0 0 0 -xx' -yx' | |a|   |x'| 
  --| 0 0 0 x y 1 -xy' -yy' | |b|   |y'|
  --| x y 1 0 0 0 -xx' -yx' | |c|   |x'|
  --| 0 0 0 x y 1 -xy' -yy' | |d| = |y'|
  --| x y 1 0 0 0 -xx' -yx' | |e|   |x'|
  --| 0 0 0 x y 1 -xy' -yy' | |f|   |y'|
  --| x y 1 0 0 0 -xx' -yx' | |g|   |x'|
  --| 0 0 0 x y 1 -xy' -yy' | |h|   |y'|

  --x = inverse(A)*b
  --inv :: Field t => Matrix t -> Matrix t

-- we need 
-- (c -> a -> d)
-- a :: point
-- a -> homography point
-- ha -> transformed point

-- model -> point -> fit value
-- homography + point -> new point
-- find nearest point in second set of points, compare feature differences weighted by pixel distance

-- (d -> Bool)
-- fit value -> Inlier?

-- Needed preprocessing functions
-- points -> v a

--test a = ransac a

--homogenizePoints :: [DIM2] -> V.Vector v a

---- residual function
--residual :: (V.Vector v a) => v a -> Maybe c -> a -> d -- get images in type signiture
--residual firstImage secondImage secondImagePoints homography point 
---- = -- value (basically, search through secondImagePoint for best match, use features)


--makeResidual :: (V.Vector v a) => v a -> (Maybe c -> a -> d) -- get images in type signiture
--makeResidual firstImage secondImage secondImagePoints = residual irstImage secondImage secondImagePoints

