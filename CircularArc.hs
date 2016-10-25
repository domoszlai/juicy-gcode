module CircularArc ( CircularArc (..)
                   , isClockwise
                   , pointAt
                   , arcLength
                   ) where
          
import Linear    
import Control.Lens

data CircularArc = CircularArc { _c :: V2 Double
                               , _r :: Double
                               , _startAngle :: Double
                               , _sweepAngle :: Double
                               , p1 :: V2 Double
                               , p2 :: V2 Double
                               } deriving Show

isClockwise :: CircularArc -> Bool
isClockwise arc = _sweepAngle arc > 0
    
pointAt :: CircularArc -> Double -> V2 Double
pointAt arc t = V2 x y
    where
        x = _c arc ^. _x + _r arc * cos (_startAngle arc + t * _sweepAngle arc)
        y = _c arc ^. _y + _r arc * sin (_startAngle arc + t * _sweepAngle arc)

arcLength :: CircularArc -> Double
arcLength arc = _r arc * abs(_sweepAngle arc)
        
-----------------------------------------------------------------------------    
-- just a very basic test
        
main = do
    print (show (pointAt (CircularArc (V2 925.8066 (-482.5004)) 978.8121 2.31507039 (-0.1437712) (V2 262.722046 237.4941) (V2 372.7215 325.070068)) 0.03553029))
