module Line ( Line (..)
            , throughPoint
            , fromPoints
            , createPerpendicularAt
            , slope
            , intersection
            , isOnLine
            ) where
          
import Linear    
import Control.Lens

-- TODO: letting _p to be NaN is actually a really bad idea
data Line = Line { _m :: Double
                 , _p :: V2 Double
                 } deriving Show
            
throughPoint :: V2 Double -> Double -> Line
throughPoint p m = Line m p
            
fromPoints :: V2 Double -> V2 Double -> Line
fromPoints p1 p2 = throughPoint p1 (slope p1 p2)
          
-- Creates a a line which is perpendicular to the line defined by P and P1 and goes through P          
createPerpendicularAt :: V2 Double -> V2 Double -> Line
createPerpendicularAt p p1
    | m == 0
        = throughPoint p nan
    | isNaN m
        = throughPoint p 0
    | otherwise 
        = throughPoint p (-1 / m)
    where
        m = slope p p1
          
slope :: V2 Double -> V2 Double -> Double
slope p1 p2 
    | p2 ^. _x == p1 ^. _x
         = nan
    | otherwise
        = (p2 ^. _y - p1 ^. _y) / (p2 ^. _x - p1 ^. _x)
   
nan :: Double   
nan = 0/0   
   
-- If the solution is not found it actually returns +/-infinity
intersection :: Line -> Line -> V2 Double
intersection line1 line2 
    | isNaN (_m line1)
        = verticalIntersection line1 line2 
    | isNaN (_m line2)
        = verticalIntersection line2 line1  
    | otherwise
        = V2 x y
    where
        x = (_m line1 * _p line1 ^. _x - _m line2 * _p line2 ^. _x - _p line1 ^. _y + _p line2 ^. _y) / (_m line1 - _m line2) 
        y = _m line1 * x - _m line1 * _p line1 ^. _x + _p line1 ^. _y
    
-- First line is vertical
verticalIntersection :: Line -> Line -> V2 Double    
verticalIntersection vline line = V2 x y
    where
        x = _p vline ^. _x
        y = _m line * (x - _p line ^. _x) + _p line ^. _y

isOnLine :: Line -> V2 Double -> Bool
isOnLine l p2 
    | isNaN (_m l)
        = p1 ^. _x == p2 ^. _x
    | otherwise 
        = (p2 ^. _x - p1 ^. _x) * (_m l) == (p2 ^. _y - p1 ^. _y) 
    where
        p1 = _p l