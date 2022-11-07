module Approx.Linear ( 
    bezier2lines
) where

import qualified Graphics.CubicBezier as B    
import qualified Graphics.Line as L
import Graphics.LineSegment

import Linear

import Error

-- Approximate a bezier curve with a series of points (line segments)
bezier2lines :: B.CubicBezier 
             -> Double
             -> [V2 Double]
bezier2lines mbezier resolution 
    -- Edge case: all points on the same line -> it is a line 
    | (L.isOnLine (L.fromPoints (B._p2 mbezier) (B._p1 mbezier)) (B._c1 mbezier)) && 
      (L.isOnLine (L.fromPoints (B._p2 mbezier) (B._p1 mbezier)) (B._c2 mbezier)) 
        = [B._p2 mbezier]
    -- Just a regular bezier
    | otherwise 
        = approx mbezier
    where         
        -- TODO: make it tail recursive
        approx :: B.CubicBezier -> [V2 Double]
        approx bezier
            -- Approximate bezier length. if max length is smaller than resolution, do not approximate
            | B.maxArcLength bezier < resolution
                = [B._p2 bezier]
            -- Edge case: start- and endpoints are the same
            | (B._p1 bezier) == (B._p2 bezier)
                = splitAndRecur 0.5
            -- Approximation is not close enough yet, refine
            | maxDistance > resolution
                = splitAndRecur maxDistanceAt
            | otherwise
                = [B._p2 bezier]

            where                                
                (maxDistance, maxDistanceAt) = calculateDistance (fromPoints (B._p1 bezier) (B._p2 bezier)) bezier

                splitAndRecur t = let (b1, b2) = B.splitAt bezier t
                                   in approx b1 ++ approx b2  

