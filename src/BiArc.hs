module BiArc ( BiArc (..)
             , create
             , pointAt
             , arcLength
             , isStable
             ) where
      
import qualified CircularArc as CA
import qualified Line as L

import Linear hiding (angle)   
import Control.Lens

data BiArc = BiArc { _a1 :: CA.CircularArc
                   , _a2 :: CA.CircularArc
                   } deriving Show
    
create :: V2 Double -- Start point
       -> V2 Double -- Tangent vector at start point
       -> V2 Double -- End point
       -> V2 Double -- Tangent vector at end point
       -> V2 Double -- Transition point (connection point of the arcs)    
       -> BiArc 
create p1 t1 p2 t2 t 
    = BiArc (CA.CircularArc c1 r1 startAngle1 sweepAngle1 p1 t) (CA.CircularArc c2 r2 startAngle2 sweepAngle2 t p2)
    where
        -- Calculate the orientation
        osum = (t ^. _x - p1 ^. _x) * (t ^. _y + p1 ^. _y)
             + (p2 ^. _x - t ^. _x) * (p2 ^. _y + t ^. _y)
             + (p1 ^. _x - p2 ^. _x) * (p1 ^. _y + p2 ^. _y)
        cw = osum  < 0
        
        -- Calculate perpendicular lines to the tangent at P1 and P2
        tl1 = L.createPerpendicularAt p1 (p1 + t1)
        tl2 = L.createPerpendicularAt p2 (p2 + t2)
        
        -- Calculate the perpendicular bisector of P1T and P2T
        p1t2 = (p1 + t) ^/ 2
        pb_p1t = L.createPerpendicularAt p1t2 t
            
        p2t2 = (p2 + t) ^/ 2
        pb_p2t = L.createPerpendicularAt p2t2 t           
            
        -- The origo of the circles are at the intersection points
        c1 = L.intersection tl1 pb_p1t
        c2 = L.intersection tl2 pb_p2t          
            
        -- Calculate the radii
        r1 = distance c1 p1
        r2 = distance c2 p2        
            
        -- Calculate start and sweep angles
        startVector1 = p1 - c1;
        endVector1 = t - c1;
        startAngle1 = atan2 (startVector1 ^. _y) (startVector1 ^. _x)
        sweepAngle1' = (atan2 (endVector1 ^. _y) (endVector1 ^. _x)) - startAngle1

        startVector2 = t - c2
        endVector2 = p2 - c2
        startAngle2 = atan2 (startVector2 ^. _y) (startVector2 ^. _x)
        sweepAngle2' = (atan2 (endVector2 ^. _y) (endVector2 ^. _x)) - startAngle2
        
        -- Adjust angles according to the orientation of the curve
        sweepAngle1 = adjustSweepAngle cw sweepAngle1'
        sweepAngle2 = adjustSweepAngle cw sweepAngle2'
        
adjustSweepAngle :: Bool -> Double -> Double
adjustSweepAngle True angle | angle < 0 = 2 * pi + angle
adjustSweepAngle False angle | angle > 0 = angle - 2 * pi
adjustSweepAngle _ angle = angle    
    
pointAt :: BiArc -> Double -> V2 Double
pointAt arc t
    | t <= s
        = CA.pointAt (_a1 arc) (t / s)
    | otherwise
        = CA.pointAt (_a2 arc) ((t - s) / (1 - s))
    where
        s = CA.arcLength (_a1 arc) / (arcLength arc)

arcLength :: BiArc -> Double
arcLength arc = CA.arcLength (_a1 arc) + CA.arcLength (_a2 arc)

-- Heuristics for unstable biarc: the radius of at least one of the arcs 
-- is too big or too small 
isStable :: BiArc -> Bool
isStable biarc
    = not (CA._r (_a1 biarc) > 99999 || CA._r (_a1 biarc) < 0.001 ||
           CA._r (_a2 biarc) > 99999 || CA._r (_a2 biarc) < 0.001)
        
