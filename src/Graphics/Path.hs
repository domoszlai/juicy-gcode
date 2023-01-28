module Graphics.Path ( 
      PathCommand(..),
      ColoredPath(..)
) where

import Graphics.Point
import Codec.Picture (PixelRGBA8 (..))

data ColoredPath = ColoredPath (Maybe PixelRGBA8) [PathCommand]

-- all of them are invariant under affine transformation
data PathCommand 
    = MoveTo Point
    | LineTo Point                 -- End point
    | ArcTo Point Point Bool       -- Center point, end point, clockwise
    | BezierTo Point Point Point   -- Control point1, control point2, end point
    deriving Show