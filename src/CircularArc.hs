module CircularArc ( CircularArc (..)
                   , isClockwise
                   , arcLength
                   ) where
          
import Linear    
import Control.Lens

import Geom

data CircularArc = CircularArc { _c :: V2 Double
                               , _r :: Double
                               , _startAngle :: Double
                               , _sweepAngle :: Double
                               , _p1 :: V2 Double
                               , _p2 :: V2 Double
                               } deriving Show

instance Curve CircularArc where
    pointAt arc t = V2 x y
        where
            x = _c arc ^. _x + _r arc * cos (_startAngle arc + t * _sweepAngle arc)
            y = _c arc ^. _y + _r arc * sin (_startAngle arc + t * _sweepAngle arc)

isClockwise :: CircularArc -> Bool
isClockwise arc = _sweepAngle arc > 0
    
arcLength :: CircularArc -> Double
arcLength arc = _r arc * abs(_sweepAngle arc)
        