module Graphics.Point ( 
      Point
    , toPoint
    , fromPoint
) where

import Linear

type Point = (Double, Double) -- A point in the plane, absolute coordinates

toPoint :: V2 Double -> Point
toPoint (V2 x y) = (x, y)

fromPoint :: Point -> V2 Double
fromPoint (x, y) = (V2 x y)