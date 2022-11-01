module Error (
    calculateDistance
) where

import Linear

import Graphics.Curve
import Utils

-- Calculate the maximum distance between two curves
-- TODO: we only calculate the distance at 8 points (first and last skipped as 
--       they should be precise), based on experiment seems a resonable approximation as of now
parameterStep :: Double
parameterStep = 1 / 10

calculateDistance :: (Curve a, Curve b) => a -> b -> (Double, Double) 
calculateDistance curve1 curve2 = maxDistance' 0 0 parameterStep
    where
        maxDistance' m mt t
            | t < 1 
                = if' (d > m) (maxDistance' d t nt) (maxDistance' m mt nt)
            | otherwise
                = (m, mt)
            where
                d = distance (pointAt curve1 t) (pointAt curve2 t)
                nt = t + parameterStep
