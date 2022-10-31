module Graphics ( 
      DrawOp(..)
    , transformDrawOp
) where

import Graphics.Point
import Graphics.Transformation

transformDrawOp :: TransformationMatrix -> DrawOp -> DrawOp
transformDrawOp m (DMoveTo p) = DMoveTo (transformPoint m p)
transformDrawOp m (DLineTo p) = DLineTo (transformPoint m p)
transformDrawOp m (DBezierTo c1 c2 p2) = DBezierTo (transformPoint m c1) (transformPoint m c2) (transformPoint m p2)

-- all of them are invariant under affine transformation
data DrawOp = DMoveTo Point
            | DLineTo Point                 -- End point
            | DBezierTo Point Point Point   -- Control point1, control point2, end point
              deriving Show
