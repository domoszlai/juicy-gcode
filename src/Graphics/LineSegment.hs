module Graphics.LineSegment (
    fromPoints
) where

import Linear

import Graphics.Curve

data LineSegment = LineSegment { _p1 :: V2 Double
                               , _p2 :: V2 Double
                               } deriving Show

fromPoints :: V2 Double -> V2 Double -> LineSegment
fromPoints p1 p2 = LineSegment p1 p2

instance Curve LineSegment where
    pointAt ls t = (_p1 ls) + (((_p2 ls) - (_p1 ls))  ^* t)     