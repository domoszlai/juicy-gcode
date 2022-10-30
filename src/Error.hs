module Error (calculateError) where

import Linear

import Geom
import Utils

-- Calculate the error
-- TODO: we only calculate the distance at 8 points (first and last skipped as 
--       they should be precise), based on experiment seems a resonable approximation as of now
parameterStep :: Double
parameterStep = 1 / 10

calculateError :: (Curve a, Curve b) => a -> b -> (Double, Double) 
calculateError curve1 curve2 = maxError' 0 0 parameterStep
    where
        maxError' m mt t
            | t < 1 
                = if' (d > m) (maxError' d t nt) (maxError' m mt nt)
            | otherwise
                = (m, mt)
            where
                d = distance (pointAt curve1 t) (pointAt curve2 t)
                nt = t + parameterStep
