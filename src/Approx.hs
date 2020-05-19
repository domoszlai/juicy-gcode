module Approx ( bezier2biarc
              ) where

import qualified CubicBezier as B
import qualified BiArc as BA          
import qualified Line as L 
          
import Data.Bool (bool)

import Linear    
import Data.Complex

import Types

bezier2biarc :: B.CubicBezier 
             -> Double
             -> Double
             -> [BA.BiArc]
bezier2biarc mbezier samplingStep tolerance
    | (B._p1 mbezier) == (B._c1 mbezier)
        = approxOne mbezier
    | (B._p2 mbezier) == (B._c2 mbezier)
        = approxOne mbezier
    | otherwise 
        = byInflection (B.realInflectionPoint i1) (B.realInflectionPoint i2)
    where
        (i1, i2) = B.inflectionPoints mbezier

        order a b | b < a = (b, a)
                  | otherwise = (a, b)
    
        byInflection True False = approxOne b1 ++ approxOne b2
            where
                (b1, b2) = B.bezierSplitAt mbezier (realPart i1)

        byInflection False True = approxOne b1 ++ approxOne b2
            where
                (b1, b2) = B.bezierSplitAt mbezier (realPart i2)
    
        byInflection True True = approxOne b1 ++ approxOne b2 ++ approxOne b3
            where
                (it1, it2') = order (realPart i1) (realPart i2)
                
                -- Make the first split and save the first new curve. The second one has to be splitted again
                -- at the recalculated t2 (it is on a new curve)                
                it2 = (1 - it1) * it2'        
                
                (b1, toSplit) = B.bezierSplitAt mbezier it1
                (b2, b3) = B.bezierSplitAt toSplit it2

        byInflection False False = approxOne mbezier
         
        -- TODO: make it tail recursive
        approxOne :: B.CubicBezier -> [BA.BiArc]
        approxOne bezier
            -- Edge case: start- and endpoints are the same
            | (B._p1 bezier) == (B._p2 bezier)
                = splitAndRecur 0.5
            -- Edge case: control lines are parallel
            | (L._m t1) == (L._m t2)
                = splitAndRecur 0.5
            -- Approximation is not close enough yet, refine
            | maxDistance > tolerance
                = splitAndRecur maxDistanceAt
            | otherwise
                = [biarc] 
            where
                -- Edge case: P1==C1 or P2==C2
                -- there is no derivative at P1 or P2, use the other control point
                c1 = bool (B._c1 bezier) (B._c2 bezier) ((B._p1 bezier) == (B._c1 bezier))
                c2 = bool (B._c2 bezier) (B._c1 bezier) ((B._p2 bezier) == (B._c2 bezier))

                -- V: Intersection point of tangent lines
                t1 = L.fromPoints (B._p1 bezier) c1
                t2 = L.fromPoints (B._p2 bezier) c2
                v = L.intersection t1 t2

                -- G: incenter point of the triangle (P1, V, P2)
                dP2V = distance (B._p2 bezier) v
                dP1V = distance (B._p1 bezier) v
                dP1P2 = distance (B._p1 bezier) (B._p2 bezier)
                g = (dP2V *^ B._p1 bezier + dP1V *^ B._p2 bezier + dP1P2 *^ v) ^/ (dP2V + dP1V + dP1P2)

                -- Calculate the BiArc
                biarc = BA.create (B._p1 bezier) (B._p1 bezier - c1) (B._p2 bezier) (B._p2 bezier - c2) g
                
                -- calculate the error
                nrPointsToCheck = (BA.arcLength biarc) / samplingStep
                parameterStep = 1 / nrPointsToCheck
                                
                (maxDistance, maxDistanceAt) = maxDistance' 0 0 0
                
                maxDistance' m mt t 
                    | t <= 1
                        = if' (d > m) (maxDistance' d t nt) (maxDistance' m mt nt)
                    | otherwise
                        = (m, mt)
                    where
                        d = distance (BA.pointAt biarc t) (B.pointAt bezier t)
                        nt = t + parameterStep

                splitAndRecur t = let (b1, b2) = B.bezierSplitAt bezier t
                                   in approxOne b1 ++ approxOne b2  

