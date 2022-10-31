module Graphics ( 
      Point
    , toPoint
    , fromPoint
    , DrawOp(..)
    , Curve(..)
) where

import Linear

type Point = (Double, Double) -- A point in the plane, absolute coordinates

toPoint :: V2 Double -> Point
toPoint (V2 x y) = (x, y)

fromPoint :: Point -> V2 Double
fromPoint (x, y) = (V2 x y)

-- all of them are invariant under affine transformation
data DrawOp = DMoveTo Point
            | DLineTo Point                 -- End point
            | DBezierTo Point Point Point   -- Control point1, control point2, end point
              deriving Show

class Curve c where
  pointAt :: c -> Double -> V2 Double