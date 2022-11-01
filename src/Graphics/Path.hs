module Graphics.Path ( 
      Path(..)
) where

import Graphics.Point

-- all of them are invariant under affine transformation
data Path = MoveTo Point
          | LineTo Point                 -- End point
          | ArcTo Point Point Bool       -- Center point, end point, clockwise
          | BezierTo Point Point Point   -- Control point1, control point2, end point
          deriving Show
