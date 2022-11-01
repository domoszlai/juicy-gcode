module Graphics.Transformation ( 
      TransformationMatrix
    , identityTransform
    , mirrorYTransform
    , translateTransform
    , scaleTransform
    , transformPoint
    , transformPath
    , applyTransformations
    , multiply
  ) where

import qualified Graphics.Svg as SVG
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
multiply a b = multStd a b

fromElements :: [Double] -> TransformationMatrix
fromElements [a,b,c,d,e,f] = fromList 3 3 [a,c,e,b,d,f,0,0,1]
fromElements _ = error "Malformed transformation matrix"

transformPoint :: TransformationMatrix -> Point -> Point
transformPoint m (x,y) = (a * x + c * y + e, b * x + d * y + f)
   where
     (a:c:e:b:d:f:_) = M.toList m

transformPath :: TransformationMatrix -> Path -> Path
transformPath m (MoveTo p) = MoveTo (transformPoint m p)
transformPath m (LineTo p) = LineTo (transformPoint m p)
transformPath m (ArcTo p1 p2 d) = ArcTo (transformPoint m p1) (transformPoint m p2) d
transformPath m (BezierTo c1 c2 p2) = BezierTo (transformPoint m c1) (transformPoint m c2) (transformPoint m p2)

applyTransformations :: TransformationMatrix -> Maybe [SVG.Transformation] -> TransformationMatrix
applyTransformations m Nothing = m
applyTransformations m (Just ts) = foldl applyTransformation m ts

radiansPerDegree :: Double
radiansPerDegree = pi / 180.0

-- https://developer.mozilla.org/en/docs/Web/SVG/Attribute/transform
applyTransformation :: Matrix Double -> SVG.Transformation -> Matrix Double
applyTransformation m (SVG.TransformMatrix a b c d e f) = multStd m (fromElements [a,b,c,d,e,f])
applyTransformation m (SVG.Translate x y) = multStd m (fromElements [1,0,0,1,x,y])
applyTransformation m (SVG.Scale sx mbSy) = multStd m (fromElements [sx,0,0,maybe sx id mbSy,0,0])
applyTransformation m (SVG.Rotate a Nothing)
    = multStd m (fromElements [cos(r),sin(r),-sin(r),cos(r),0,0])
    where
        r = a * radiansPerDegree
applyTransformation m (SVG.Rotate a (Just (x, y))) = applyTransformations m (Just [SVG.Translate x y , SVG.Rotate a Nothing , SVG.Translate (-x) (-y)])
applyTransformation m (SVG.SkewX a) = multStd m (fromElements [1,0,tan(a*radiansPerDegree),1,0,0])
applyTransformation m (SVG.SkewY a) = multStd m (fromElements [1,tan(a*radiansPerDegree),0,1,0,0])
applyTransformation m (SVG.TransformUnknown) = m
