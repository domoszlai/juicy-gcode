module Approx.Linear (
    linearApprox
) where

import qualified Graphics.CubicBezier as B
import qualified Graphics.Line as L
import Graphics.Path
import Graphics.Point

import Linear.Metric

-- Approximate a bezier curve with a series of points (line segments)
-- Weiyin Ma, Renjiang Zhang, Efficient Piecewise Linear Approximation of Bézier Curves with Improved Sharp Error Bound (2006)
linearApprox :: B.CubicBezier
             -> Double
             -> [PathCommand]
linearApprox mbezier resolution
    -- Edge case: all points on the same line -> it is a line 
    | L.isOnLine (L.fromPoints (B._p2 mbezier) (B._p1 mbezier)) (B._c1 mbezier) &&
      L.isOnLine (L.fromPoints (B._p2 mbezier) (B._p1 mbezier)) (B._c2 mbezier)
        = [LineTo (toPoint (B._p2 mbezier))]
    -- Just a regular bezier
    | otherwise
        = approx mbezier
    where
        approx :: B.CubicBezier -> [PathCommand]
        approx bezier
            | maxError > resolution
                = splitAndRecur 0.5
            | otherwise
                = [LineTo (toPoint b1), LineTo (toPoint b2), LineTo (toPoint b3)]

            where
                b1 = ((9 * B._p1 bezier) + (15 * B._c1 bezier) + (7 * B._c2 bezier) + B._p2 bezier) / 32
                b2 = (B._p1 bezier + (7 * B._c1 bezier) + (15 * B._c2 bezier) + (9 * B._p2 bezier)) / 32
                b3 = B._p2 bezier

                maxError = max (norm (B._p1 bezier - 2 * B._c1 bezier + B._c2 bezier))
                               (norm (B._c1 bezier - 2 * B._c2 bezier + B._p2 bezier))

                splitAndRecur t = let (nb1, nb2) = B.splitAt bezier t
                                   in approx nb1 ++ approx nb2