module Approx ( bezier2biarc
              ) where
                    
import qualified CubicBezier as B
import qualified BiArc as BA          
import qualified Line as L 
          
import Linear    
import Control.Lens
import Data.Complex

iif True t f = t 
iif False t f = f   


bezier2biarc :: B.CubicBezier 
             -> Double
             -> Double
             -> [BA.BiArc]
bezier2biarc bezier samplingStep tolerance
    = byInflection (B.realInflectionPoint i1) (B.realInflectionPoint i2)
    where        
        (i1, i2) = B.inflectionPoints bezier
    
        order a b | b < a = (b, a)
                  | otherwise = (a, b)
    
        byInflection True False = approxOne b1 ++ approxOne b2
            where
                (b1, b2) = B.bezierSplitAt bezier (realPart i1)

        byInflection False True = approxOne b1 ++ approxOne b2
            where
                (b1, b2) = B.bezierSplitAt bezier (realPart i2)
    
        byInflection True True = approxOne b1 ++ approxOne b2 ++ approxOne b3
            where
                (it1, it2') = order (realPart i1) (realPart i2)
                
                -- Make the first split and save the first new curve. The second one has to be splitted again
                -- at the recalculated t2 (it is on a new curve)                
                it2 = (1 - it1) * it2'        
                
                (b1, toSplit) = B.bezierSplitAt bezier it1
                (b2, b3) = B.bezierSplitAt toSplit it2

        byInflection False False = approxOne bezier
         
        -- TODO: make it tail recursive
        approxOne :: B.CubicBezier -> [BA.BiArc]
        approxOne bezier 
            | maxDistance > tolerance
                = let (b1, b2) = B.bezierSplitAt bezier maxDistanceAt 
                   in approxOne b1 ++ approxOne b2
            | otherwise
                = [biarc]
            where
                -- V: Intersection point of tangent lines
                t1 = L.fromPoints (B._p1 bezier) (B._c1 bezier)
                t2 = L.fromPoints (B._p2 bezier) (B._c2 bezier)
                v = L.intersection t1 t2
        
                -- G: incenter point of the triangle (P1, V, P2)
                dP2V = distance (B._p2 bezier) v
                dP1V = distance (B._p1 bezier) v
                dP1P2 = distance (B._p1 bezier) (B._p2 bezier)
                g = (dP2V *^ B._p1 bezier + dP1V *^ B._p2 bezier + dP1P2 *^ v) ^/ (dP2V + dP1V + dP1P2)

                -- Calculate the BiArc
                biarc = BA.create (B._p1 bezier) (B._p1 bezier - B._c1 bezier) (B._p2 bezier) (B._p2 bezier - B._c2 bezier) g
                
                -- calculate the error
                nrPointsToCheck = (BA.arcLength biarc) / samplingStep
                parameterStep = 1 / nrPointsToCheck
                                
                (maxDistance, maxDistanceAt) = maxDistance' 0 0 0
                
                maxDistance' m mt t 
                    | t <= 1
                        = iif (d > m) (maxDistance' d t nt) (maxDistance' m mt nt)
                    | otherwise
                        = (m, mt)
                    where
                        d = distance (BA.pointAt biarc t) (B.pointAt bezier t)
                        nt = t + parameterStep
        
-----------------------------------------------------------------------------    
-- just a very basic test

b1 = B.CubicBezier (V2 100 500) (V2 150 100) (V2 500 150) (V2 350 350)
        
main = do
    print (show (bezier2biarc b1 5 1))
