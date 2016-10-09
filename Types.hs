
module Types ( Point
             , DArcDir
             , DrawOp (..)
             ) where

-- type Command = String
type Point = (Double,Double) -- A point in the plane, absolute coordinates

data DArcDir = CC | CCW deriving Show

-- all of them are invariant under affine transformation
data DrawOp = DMoveTo Point                 
            | DLineTo Point                 -- End point
            | DBezierTo Point Point Point     -- Control point1, control point2, end point 
              deriving Show
