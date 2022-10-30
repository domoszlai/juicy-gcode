module CubicBezier ( CubicBezier (..)
                   , pointAt
                   , bezierSplitAt
                   , isClockwise
                   , inflectionPoints
                   ) where

import Linear                   
import Control.Lens
import Data.Complex
                   
data CubicBezier = CubicBezier { _p1 :: V2 Double
                               , _c1 :: V2 Double
                               , _c2 :: V2 Double
                               , _p2 :: V2 Double
                               } deriving Show
                               
pointAt :: CubicBezier -> Double -> V2 Double
pointAt bezier t =  ((1 - t) ** 3) *^ _p1 bezier + 
                    ((1 - t) ** 2) * 3 * t *^ _c1 bezier +
                    (t ** 2) * (1 - t) * 3 *^ _c2 bezier +
                    (t ** 3) *^ _p2 bezier
                               
bezierSplitAt :: CubicBezier -> Double -> (CubicBezier, CubicBezier)
bezierSplitAt bezier t = (CubicBezier (_p1 bezier) p0 p01 dp, CubicBezier dp p12 p2 (_p2 bezier))
    where
        p0 = _p1 bezier + t *^ (_c1 bezier - _p1 bezier)
        p1 = _c1 bezier + t *^ (_c2 bezier - _c1 bezier)        
        p2 = _c2 bezier + t *^ (_p2 bezier - _c2 bezier)   
        
        p01 = p0 + t *^ (p1 - p0)                       
        p12 = p1 + t *^ (p2 - p1)  

        dp = p01 + t *^ (p12 - p01)  
       
isClockwise :: CubicBezier -> Bool
isClockwise bezier = s < 0
    where
        s = (_c1 bezier ^. _x - _p1 bezier  ^. _x) * (_c1 bezier ^. _y + _p1 bezier ^. _y)
          + (_c2 bezier ^. _x - _c1 bezier  ^. _x) * (_c2 bezier ^. _y + _c1 bezier ^. _y)
          + (_p2 bezier ^. _x - _c2 bezier  ^. _x) * (_p2 bezier ^. _y + _c2 bezier ^. _y)
          + (_p1 bezier ^. _x - _p2 bezier  ^. _x) * (_p1 bezier ^. _y + _p2 bezier ^. _y)
    
inflectionPoints :: CubicBezier -> [Double]
inflectionPoints bezier
    | a /= 0 = realInflectionPoints [t1, t2]
    | otherwise = realInflectionPoints [t]
    where
        pa = _c1 bezier - _p1 bezier
        pb = _c2 bezier - _c1 bezier - pa
        pc = _p2 bezier - _c2 bezier - pa - 2 *^ pb
        
        a = (pb ^. _x * pc ^. _y - pb ^. _y * pc ^. _x) :+ 0
        b = (pa ^. _x * pc ^. _y - pa ^. _y * pc ^. _x) :+ 0
        c = (pa ^. _x * pb ^. _y - pa ^. _y * pb ^. _x) :+ 0
        
        -- linear case
        t = -c / b

        -- quadratic case
        t1 = (-b + sqrt (b * b  - 4 * a * c)) / (2 * a)
        t2 = (-b - sqrt (b * b  - 4 * a * c)) / (2 * a)

realInflectionPoints :: [Complex Double] -> [Double]
realInflectionPoints = map realPart . filter isInflectionPoint

isInflectionPoint :: Complex Double -> Bool
isInflectionPoint c = imagPart c == 0 && realPart c > 0 && realPart c < 1
