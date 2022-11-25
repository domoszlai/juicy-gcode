{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Approx.BiArc (
    bezier2biarcs
) where

import qualified Graphics.CubicBezier as B
import qualified Graphics.BiArc as BA
import qualified Graphics.CircularArc as CA
import qualified Graphics.Line as L
import Graphics.Curve
import Graphics.Path
import Graphics.Point
import Utils

import Data.Bool (bool)
import Control.Lens
import Linear

import Debug.Trace

eps :: Double
eps = 0.0001
maxiter :: Double
maxiter = 10

-- Approximate a bezier curve with biarcs (Left) and line segments (Right)
bezier2biarcs :: B.CubicBezier
              -> Double
              -> [PathCommand]
bezier2biarcs mbezier resolution
    -- Degenerate curve: all points on the same line -> it is a line 
    | L.isOnLine (L.fromPoints (B._p2 mbezier) (B._p1 mbezier)) (B._c1 mbezier) &&
      L.isOnLine (L.fromPoints (B._p2 mbezier) (B._p1 mbezier)) (B._c2 mbezier)
        = [LineTo (toPoint (B._p2 mbezier))]
    -- Degenerate curve: p1 == c1, don't split
    | B._p1 mbezier == B._c1 mbezier
        = approxOne mbezier
    -- Degenerate curve: p2 == c2, don't split
    | B._p2 mbezier == B._c2 mbezier
        = approxOne mbezier
    -- Split by the inflexion points (if any)
    | otherwise
        = byInflection (B.inflectionPoints mbezier)
    where
        order a b | b < a = (b, a)
                  | otherwise = (a, b)

        byInflection [t] = approxOne b1 ++ approxOne b2
            where
                (b1, b2) = B.splitAt mbezier t

        byInflection [t1, t2] = approxOne b1 ++ approxOne b2 ++ approxOne b3
            where
                (it1, it2') = order t1 t2

                -- Make the first split and save the first new curve. The second one has to be splitted again
                -- at the recalculated t2 (it is on a new curve)                
                it2 = (1 - it1) * it2'

                (b1, toSplit) = B.splitAt mbezier it1
                (b2, b3) = B.splitAt toSplit it2

        byInflection _ = approxOne mbezier

        -- Recursive step (TODO: tail recursive) 
        approxOne :: B.CubicBezier -> [PathCommand]
        approxOne bezier
            -- Approximate bezier length. if max length is smaller than resolution, do not approximate
            | B.maxArcLength bezier < resolution
                = [LineTo (toPoint (B._p2 bezier))]
            -- Edge case: start- and endpoints are the same
            | B._p1 bezier == B._p2 bezier
                = splitAndRecur 0.5
            -- Edge case: control lines are parallel
            | L._m t1 == L._m t2 || isNaN (L._m t1) && isNaN (L._m t2)
                = splitAndRecur 0.5
            -- Biarc triangle has the wrong orientation
            -- Curve looks like this: https://pomax.github.io/bezierinfo/images/chapters/decasteljau/df92f529841f39decf9ad62b0967855a.png
            | B.isClockwise bezier /= isClockwise3 (B._p1 bezier) (B._p2 bezier) v
                = splitAndRecur 0.5
            -- Unstable approximation: split the bezier into half, it will switch to linear approximation if the segments get too small
            | not (isStable biarc)
                = Debug.Trace.trace "Buu" splitAndRecur 0.5
            -- Approximation is not close enough yet, refine
            | maxDistance > resolution
                = splitAndRecur maxDistanceAt
            -- Desired case: approximation is stable and close enough
            | otherwise
                = biarc2path biarc

            where
                -- Edge case: P1==C1 or P2==C2
                -- there is no derivative at P1 or P2, use the other control point
                c1 = bool (B._c1 bezier) (B._c2 bezier) (B._p1 bezier == B._c1 bezier)
                c2 = bool (B._c2 bezier) (B._c1 bezier) (B._p2 bezier == B._c2 bezier)

                -- V: Intersection point of tangent lines
                t1 = L.fromPoints (B._p1 bezier) c1
                t2 = L.fromPoints (B._p2 bezier) c2
                v = L.intersection t1 t2

                -- G: incenter point of the triangle (P1, V, P2)
                dP2V = distance (B._p2 bezier) v
                dP1V = distance (B._p1 bezier) v
                dP1P2 = distance (B._p1 bezier) (B._p2 bezier)
                g = (dP2V *^ B._p1 bezier + dP1V *^ B._p2 bezier + dP1P2 *^ v) ^/ (dP2V + dP1V + dP1P2)

                -- Calculate the BiArc
                biarc = BA.fromPoints (B._p1 bezier) (B._p1 bezier - c1) (B._p2 bezier) (B._p2 bezier - c2) g

                (maxDistanceAt, maxDistance) = calculateMaxDistance bezier biarc

                splitAndRecur t = let (b1, b2) = B.splitAt bezier t
                                   in approxOne b1 ++ approxOne b2

biarc2path :: BA.BiArc -> [PathCommand]
biarc2path biarc = map
    (\arc -> ArcTo (toPoint (CA._c arc)) (toPoint (CA._p2 arc)) (CA.isClockwise arc))
    [BA._a1 biarc, BA._a2 biarc]

-- Heuristics for unstable biarc: the radius of at least one of the arcs 
-- is too big or too small. Not too scientific...
isStable :: BA.BiArc -> Bool
isStable biarc
    = not (CA._r (BA._a1 biarc) > 99999 || CA._r (BA._a1 biarc) < 0.001 ||
           CA._r (BA._a2 biarc) > 99999 || CA._r (BA._a2 biarc) < 0.001)

-- Calculate the maximum approximation error along the radial direction
-- D.J. Walton*, D.S. Meek, Approximation of a planar cubic Bezier spiral by circular arcs (1996)
calculateMaxDistance :: B.CubicBezier -> BA.BiArc -> (Double, Double)
calculateMaxDistance bezier biarc
    -- This should not happenm but if, split the bezier at the middle
    | tj == -1 = (0.5, 0x7FEFFFFFFFFFFFFF)
    | otherwise = bigger (bigger (tj, dj) (t0, d0)) (t1, d1)
    where
        tj = findRadialIntersection bezier biarc (BA.jointAt biarc)
        dj = distance (pointAt bezier tj) (pointAt biarc (BA.jointAt biarc))

        g arc u = dot (pointAt bezier u - CA._c arc) (B.firstDerivativeAt bezier u)
        g' arc u = quadrance (B.firstDerivativeAt bezier u) +
                   dot (pointAt bezier u - CA._c arc) (B.secondDerivativeAt bezier u)

        bigger f@(_, df) s@(ts, ds)
            | ts == -1 = f
            | df > ds = f
            | otherwise = s

        -- Valid in (0,tj]
        t0 = findRoot (g (BA._a1 biarc)) (g' (BA._a1 biarc)) eps tj
        d0 = abs ((distance (pointAt bezier t0) (CA._c (BA._a1 biarc))) - (CA._r (BA._a1 biarc)))
        -- Valid in [tj,1)
        t1 = findRoot (g (BA._a2 biarc)) (g' (BA._a2 biarc)) tj (1 - eps)
        d1 = abs ((distance (pointAt bezier t1) (CA._c (BA._a2 biarc))) - (CA._r (BA._a2 biarc)))

-- Takes a paramater `t` fore the `biarc` and calculates the the related parameter fo
-- the `bezier` (which is the intersection point in the radial direction)
findRadialIntersection :: B.CubicBezier -> BA.BiArc -> Double -> Double
findRadialIntersection bezier biarc t
    | t == 0 || t == 1 = t
    | otherwise = findRoot (\u -> dot (pointAt bezier u - p) h) (\u -> dot (B.firstDerivativeAt bezier u) h) 0 1
    where
        p = pointAt biarc t
        c = CA._c $ if' (t <= BA.jointAt biarc) (BA._a1 biarc) (BA._a2 biarc)
        m = p - c
        h = normalize $ V2 (negate (m ^. _y)) (m ^. _x)

-- Tries to find the root of f in interval [lowerBound,upperBound] using a combination of
-- Newton and bisection methods.
-- It is supposed to have at most one solution. If no solution is found, returns -1
findRoot :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double
findRoot f df lowerBound upperBound
    | fl * fu > 0 = -1
    | fl == 0 = lowerBound
    | fu == 0 = upperBound
    | otherwise = iter maxiter fl fu lowerBound upperBound ((lowerBound + upperBound) / 2)
    where
        fl = f lowerBound
        fu = f upperBound

        iter i fmin fmax lb ub root
            -- we're good, or if i==0, we may not reached tolarence yet, but hopefully it is close enough
            | abs fx < eps || i <= 0 = root
            -- overshoot or undershoot -> switch to bisection
            | n < lb || n > ub
                = if' (fmin * fx < 0)
                    (iter (i-1) fmin fx lb root ((lb + root) / 2))
                    (iter (i-1) fx fmax root ub ((root + ub) / 2))
            -- Newton step
            | otherwise
                = iter (i-1) fmin fmax lb ub n
            where
                fx = f root
                h = fx / df root
                n = root - h