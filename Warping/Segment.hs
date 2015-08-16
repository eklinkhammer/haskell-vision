{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies #-}

module Warping.Segment
(       Segment (..)
    ,   Point (..)
    ,   segGetU
    ,   segGetV
    ,   uvToX
    ,   distPointToSegment
    ,   weight
    ,   pointScalarMult
    ,   pointAdd
    ,   pointSubtract
) where

import Warping.Segment.Class