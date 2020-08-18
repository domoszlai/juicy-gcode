module Types ( Point
             , DrawOp (..)
             , GCodeOp (..)
             , if'
             ) where

type Point = (Double, Double) -- A point in the plane, absolute coordinates

-- all of them are invariant under affine transformation
data DrawOp = DMoveTo Point
            | DLineTo Point                 -- End point
            | DBezierTo Point Point Point   -- Control point1, control point2, end point
              deriving Show

-- this is basically what GCode can do
data GCodeOp = GMoveTo Point
             | GLineTo Point                -- End point
             | GArcTo Point Point Bool      -- Center point, end point, clockwise
             | GBezierTo Point Point Point  -- First and second control points, end point
               deriving Show

-- just to make it available everywhere
if' :: Bool -> t -> t -> t
if' True t _ = t
if' False _ f = f
