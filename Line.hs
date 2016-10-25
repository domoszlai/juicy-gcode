module Line ( Line (..)
            , throughPoint
            , fromPoints
            , y
            , slope
            , intersection
            ) where
          
import Linear    
import Control.Lens

data Line = Line { _m :: Double
                 , _b :: Double
                 } deriving Show
            
throughPoint :: V2 Double -> Double -> Line
throughPoint p m = Line m (p ^. _y - m * p ^. _x)
            
fromPoints :: V2 Double -> V2 Double -> Line
fromPoints p1 p2 = throughPoint p1 (slope p1 p2)
            
y :: Line -> Double -> Double
y line x = _m line * x + _b line
   
slope :: V2 Double -> V2 Double -> Double
slope p1 p2 = (p2 ^. _y - p1 ^. _y) / (p2 ^. _x - p1 ^. _x)
   
intersection :: Line -> Line -> V2 Double
intersection line1 line2 = V2 x (y line1 x)
    where
        x = (_b line1 - _b line2) / (_m line2 - _m line1)
    
-----------------------------------------------------------------------------    
-- just a very basic test
        
main = do
    print (show (intersection (fromPoints (V2 262.722 237.4941) (V2 290.2062 262.8059)) (fromPoints (V2 500 400) (V2 348.4515 324.2258))))
