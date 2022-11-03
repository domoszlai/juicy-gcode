module Graphics.Path ( 
      PathCommand(..)
) where

import Graphics.Point

-- all of them are invariant under affine transformation
data PathCommand 
    = MoveTo Point
    | LineTo Point                 -- End point
    | ArcTo Point Point Bool       -- Center point, end point, clockwise
    | BezierTo Point Point Point   -- Control point1, control point2, end point
    deriving Show