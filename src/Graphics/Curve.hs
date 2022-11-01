module Graphics.Curve ( 
    Curve(..)
) where

import Linear

class Curve c where
  pointAt :: c -> Double -> V2 Double