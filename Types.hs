
module Types ( Point
             , DArcDir
             , DrawOp (..)
             , GCodeOp (..)
             ) where

-- type Command = String
type Point = (Double,Double) -- A point in the plane, absolute coordinates

data DArcDir = CC | CCW deriving Show

-- all of them are invariant under affine transformation
data DrawOp = DMoveTo Point                 
            | DLineTo Point                 -- End point
            | DBezierTo Point Point Point   -- Control point1, control point2, end point 
              deriving Show
              
-- this is basically what GCode can do
data GCodeOp = GMoveTo Point
             | GLineTo Point                -- End point
             | GArcTo Point Point Bool      -- Center point, end point, clockwise
               deriving Show             
