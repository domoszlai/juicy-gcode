module Line ( Line (..)
            , throughPoint
            , fromPoints
            , createPerpendicularAt
            , slope
            , intersection
            ) where
          
import Linear    
import Control.Lens

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
        = throughPoint p 0
    | otherwise 
        = throughPoint p (-1 / m)
    where
        m = slope p p1
          
slope :: V2 Double -> V2 Double -> Double
slope p1 p2 
    | p2 ^. _x == p1 ^. _x
         = 0
    | otherwise
        = (p2 ^. _y - p1 ^. _y) / (p2 ^. _x - p1 ^. _x)
   
-- If the solution is not unique it actually return +/-infinity
intersection :: Line -> Line -> V2 Double
intersection line1 line2 = V2 x y
    where
        x = (_m line1 * _p line1 ^. _x - _m line2 * _p line2 ^. _x - _p line1 ^. _y + _p line2 ^. _y) / (_m line1 - _m line2) 
        y = _m line1 * x - _m line1 * _p line1 ^. _x + _p line1 ^. _y
    
-----------------------------------------------------------------------------    
-- just a very basic test
        
main = do
    print (show (intersection (fromPoints (V2 262.722 237.4941) (V2 290.2062 262.8059)) (fromPoints (V2 500 400) (V2 348.4515 324.2258))))