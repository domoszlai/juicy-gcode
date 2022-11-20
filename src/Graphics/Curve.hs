module Graphics.Curve ( 
    Curve(..),
    isClockwise4,
    isClockwise3
) where

import Linear
import Control.Lens

class Curve c where
  pointAt :: c -> Double -> V2 Double

isClockwise4 :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Bool
isClockwise4 p1 p2 p3 p4 = s < 0
    where
        s = (p2 ^. _x - p1 ^. _x) * (p2 ^. _y + p1 ^. _y)
          + (p3 ^. _x - p2 ^. _x) * (p3 ^. _y + p2 ^. _y)
          + (p4 ^. _x - p3 ^. _x) * (p4 ^. _y + p3 ^. _y)
          + (p1 ^. _x - p4 ^. _x) * (p1 ^. _y + p4 ^. _y)

isClockwise3 :: V2 Double -> V2 Double -> V2 Double -> Bool
isClockwise3 p1 p2 p3 = s < 0
    where
        s = (p3 ^. _x - p1 ^. _x) * (p3 ^. _y + p1 ^. _y)
          + (p2 ^. _x - p3 ^. _x) * (p2 ^. _y + p3 ^. _y)
          + (p1 ^. _x - p2 ^. _x) * (p1 ^. _y + p2 ^. _y)