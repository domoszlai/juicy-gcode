module Interpol.BiArc ( 
    bezier2biarcs
) where

import qualified Graphics.CubicBezier as B
import qualified Graphics.BiArc as BA          
import qualified Graphics.Line as L 
          
import Data.Bool (bool)
import Linear

import Error

-- Approximate a bezier curve with biarcs (Left) and line segments (Right)
bezier2biarcs :: B.CubicBezier 
              -> Double
              -> [Either BA.BiArc (V2 Double)]
bezier2biarcs mbezier resolution 
    -- Edge case: all points on the same line -> it is a line 
    | (L.isOnLine (L.fromPoints (B._p2 mbezier) (B._p1 mbezier)) (B._c1 mbezier)) && 
      (L.isOnLine (L.fromPoints (B._p2 mbezier) (B._p1 mbezier)) (B._c2 mbezier)) 
        = [Right (B._p2 mbezier)]
    -- Edge case: p1 == c1, don't split
    | (B._p1 mbezier) == (B._c1 mbezier)
        = approxOne mbezier
    -- Edge case: p2 == c2, don't split
    | (B._p2 mbezier) == (B._c2 mbezier)
        = approxOne mbezier
    -- Split by the inflexion points (if any)
    | otherwise 
        = byInflection (B.inflectionPoints mbezier)
    where
        order a b | b < a = (b, a)
                  | otherwise = (a, b)
    
        byInflection [t] = approxOne b1 ++ approxOne b2
            where
                (b1, b2) = B.splitAt mbezier t
    
        byInflection [t1, t2] = approxOne b1 ++ approxOne b2 ++ approxOne b3
            where
                (it1, it2') = order t1 t2
                
                -- Make the first split and save the first new curve. The second one has to be splitted again
                -- at the recalculated t2 (it is on a new curve)                
                it2 = (1 - it1) * it2'        
                
                (b1, toSplit) = B.splitAt mbezier it1
                (b2, b3) = B.splitAt toSplit it2

        byInflection _ = approxOne mbezier
         
        -- TODO: make it tail recursive
        approxOne :: B.CubicBezier -> [Either BA.BiArc (V2 Double)]
        approxOne bezier
            -- Approximate bezier length. if max length is smaller than resolution, do not approximate
            | B.maxArcLength bezier < resolution
                = [Right (B._p2 bezier)]
            -- Edge case: start- and endpoints are the same
            | (B._p1 bezier) == (B._p2 bezier)
                = splitAndRecur 0.5
            -- Edge case: control lines are parallel
            | (L._m t1) == (L._m t2) || (isNaN (L._m t1) && isNaN (L._m t2)) 
                = splitAndRecur 0.5
            -- Approximation is not close enough yet, refine
            | BA.isStable biarc && maxDistance > resolution
                = splitAndRecur maxDistanceAt
            -- Desired case: approximation is stable and close enough
            | BA.isStable biarc
                = [Left biarc]
            -- Unstable approximation: split the bezier into half, basically switching to
            -- linear approximation mode
            | otherwise
                = splitAndRecur 0.5 -- TODO: use linear if not stable

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
                biarc = BA.fromPoints (B._p1 bezier) (B._p1 bezier - c1) (B._p2 bezier) (B._p2 bezier - c2) g
                                
                (maxDistance, maxDistanceAt) = calculateDistance biarc bezier

                splitAndRecur t = let (b1, b2) = B.splitAt bezier t
                                   in approxOne b1 ++ approxOne b2 