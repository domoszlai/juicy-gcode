{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Transformation ( 
      TransformationMatrix
    , fromElements
    , identityTransform
    , mirrorYTransform
    , multiply
    , translateTransform
    , scaleTransform
    , Transform(..)
  ) where

import Data.Matrix as M

import Graphics.Point
import Graphics.Path

type TransformationMatrix = Matrix Double

identityTransform :: TransformationMatrix
identityTransform = identity 3

mirrorYTransform :: Double -> Double -> TransformationMatrix
mirrorYTransform _ h = fromElements [1, 0, 0, -1, 0, h]

translateTransform :: Double -> Double -> TransformationMatrix
translateTransform x y = fromElements [1, 0, 0, 1, x, y]

scaleTransform :: Double -> Double -> TransformationMatrix
scaleTransform sx sy = fromElements [sx, 0, 0, sy, 0, 0]

multiply :: TransformationMatrix -> TransformationMatrix -> TransformationMatrix
multiply = multStd

fromElements :: [Double] -> TransformationMatrix
fromElements [a,b,c,d,e,f] = fromList 3 3 [a,c,e,b,d,f,0,0,1]
fromElements _ = error "Malformed transformation matrix"

class Transform t where
  transform :: TransformationMatrix -> t -> t

instance Transform Point where
  transform m (x,y) = (a * x + c * y + e, b * x + d * y + f)
    where
      (a:c:e:b:d:f:_) = M.toList m

instance Transform PathCommand where
  transform m (MoveTo p) = MoveTo (transform m p)
  transform m (LineTo p) = LineTo (transform m p)
  transform m (ArcTo p1 p2 d) = ArcTo (transform m p1) (transform m p2) d
  transform m (BezierTo c1 c2 p2) = BezierTo (transform m c1) (transform m c2) (transform m p2)


