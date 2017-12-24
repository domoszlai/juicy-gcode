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
                               , _p1 :: V2 Double
                               , _p2 :: V2 Double
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
        