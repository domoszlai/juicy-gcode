module Render (
    Approximation(..),
    renderDoc
) where

import Data.Maybe ( fromMaybe )

import qualified Graphics.Svg as SVG
import qualified Graphics.Svg.CssTypes as CSS
import qualified Linear

import Graphics.Path
import Graphics.Point
import Graphics.Transformation
import Approx.BiArc
import Approx.Linear
import SvgArcSegment
import SVGExt

import qualified Graphics.CubicBezier as B

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

fromSvgPoint :: Int -> SVG.Point -> Point
fromSvgPoint dpi (x,y) = (fromSvgNumber dpi x, fromSvgNumber dpi y)

fromRPoint :: SVG.RPoint -> Point
fromRPoint (Linear.V2 x y) = (x, y)

-- TODO: em, percentage
fromSvgNumber :: Int -> SVG.Number -> Double
fromSvgNumber dpi num = fromNumber' (CSS.toUserUnit dpi num)
    where
        fromNumber' (SVG.Num n) = n
        fromNumber' _ = error "TODO: unhandled em or percentage"

-- current point + control point -> mirrored control point
mirrorControlPoint :: Point -> Point -> Point
mirrorControlPoint (cx, cy) (cpx, cpy) = (cx + cx - cpx, cy + cy - cpy)

-- convert a quadratic bezier to a cubic one
bezierQ2C :: Point -> Point -> Point -> PathCommand
bezierQ2C (qp0x, qp0y) (qp1x, qp1y) (qp2x, qp2y)
    = BezierTo (qp0x + 2.0 / 3.0 * (qp1x - qp0x), qp0y + 2.0 / 3.0 * (qp1y - qp0y))
               (qp2x + 2.0 / 3.0 * (qp1x - qp2x), qp2y + 2.0 / 3.0 * (qp1y - qp2y))
               (qp2x, qp2y)

toAbsolute :: (Double, Double) -> SVG.Origin -> (Double, Double) -> (Double, Double)
toAbsolute _ SVG.OriginAbsolute p = p
toAbsolute (cx,cy) SVG.OriginRelative (dx,dy) = (cx+dx, cy+dy)

-- Apply SVG transformations to a TransformationMatrix
applyTransformations :: TransformationMatrix -> Maybe [SVG.Transformation] -> TransformationMatrix
applyTransformations m Nothing = m
applyTransformations m (Just ts) = foldl applyTransformation m ts

radiansPerDegree :: Double
radiansPerDegree = pi / 180.0

-- https://developer.mozilla.org/en/docs/Web/SVG/Attribute/transform
applyTransformation :: TransformationMatrix -> SVG.Transformation -> TransformationMatrix
applyTransformation m (SVG.TransformMatrix a b c d e f) = multiply m (fromElements [a,b,c,d,e,f])
applyTransformation m (SVG.Translate x y) = multiply m (fromElements [1,0,0,1,x,y])
applyTransformation m (SVG.Scale sx mbSy) = multiply m (fromElements [sx,0,0,Data.Maybe.fromMaybe sx mbSy,0,0])
applyTransformation m (SVG.Rotate a Nothing)
    = multiply m (fromElements [cos r, sin r, -sin r, cos r , 0, 0])
    where
        r = a * radiansPerDegree
applyTransformation m (SVG.Rotate a (Just (x, y))) = applyTransformations m (Just [SVG.Translate x y , SVG.Rotate a Nothing , SVG.Translate (-x) (-y)])
applyTransformation m (SVG.SkewX a) = multiply m (fromElements [1,0,tan(a*radiansPerDegree),1,0,0])
applyTransformation m (SVG.SkewY a) = multiply m (fromElements [1,tan(a*radiansPerDegree),0,1,0,0])
applyTransformation m SVG.TransformUnknown = m

docTransform :: Int -> SVG.Document -> TransformationMatrix
docTransform dpi doc = multiply mirrorTransform (viewBoxTransform $ SVG._viewBox doc)
    where
        viewBoxTransform (Just (vbx,vby,vbw,vbh))
            = multiply (scaleTransform (w/vbw) (h/vbh)) (translateTransform (-vbx) (-vby))
        viewBoxTransform Nothing
            = identityTransform

        mirrorTransform = mirrorYTransform w h

        (w, h) = documentSize dpi doc

data Approximation = BiArc | CubicBezier | Linear

renderDoc :: Approximation -> Int -> Double -> SVG.Document -> [PathCommand]
renderDoc approximation dpi resolution doc
    = stage2 $ renderTrees (docTransform dpi doc) (SVG._elements doc)
    where
        pxresolution = fromIntegral dpi / 2.45 / 10 * resolution

        -- TODO: make it tail recursive
        stage2 :: [PathCommand] -> [PathCommand]
        stage2 dops = approximate dops (Linear.V2 0 0)
            where
                approximate [] _ = []
                approximate (MoveTo p:ds) _ = MoveTo p : approximate ds (fromPoint p)
                approximate (LineTo p:ds) _ = LineTo p : approximate ds (fromPoint p)
                approximate (ArcTo p1 p2 d:ds) _ = ArcTo p1 p2 d : approximate ds (fromPoint p2)
                approximate (BezierTo c1 c2 p2:ds) cp =
                    case approximation of
                        BiArc       -> bezier2biarcs
                                                (B.CubicBezier cp (fromPoint c1) (fromPoint c2) (fromPoint p2)) pxresolution
                                            ++ approximate ds (fromPoint p2)
                        CubicBezier -> BezierTo c1 c2 p2 : approximate ds (fromPoint p2)
                        Linear      -> linearApprox
                                                (B.CubicBezier cp (fromPoint c1) (fromPoint c2) (fromPoint p2)) pxresolution
                                            ++ approximate ds (fromPoint p2)

        renderPathCommands :: Point -> Point -> Maybe Point -> [SVG.PathCommand] -> [PathCommand]
        renderPathCommands _ currentp _ (SVG.MoveTo origin (p:ps):ds)
            = MoveTo ap : renderPathCommands ap ap Nothing (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)

                cont [] = ds
                cont ps' = SVG.LineTo origin ps' : ds

        renderPathCommands firstp currentp _ (SVG.LineTo origin (p:ps):ds)
            = LineTo ap : renderPathCommands firstp ap Nothing (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)

                cont [] = ds
                cont ps' = SVG.LineTo origin ps' : ds

        renderPathCommands firstp (_, cy) _ (SVG.HorizontalTo SVG.OriginAbsolute (px:pxs):ds)
            = LineTo ap : renderPathCommands firstp ap Nothing (cont pxs)
            where
                ap = (px,cy)

                cont [] = ds
                cont pxs' = SVG.HorizontalTo SVG.OriginAbsolute pxs' : ds

        renderPathCommands firstp (cx, cy) _ (SVG.HorizontalTo SVG.OriginRelative (dx:dxs):ds)
            = LineTo ap : renderPathCommands firstp ap Nothing (cont dxs)
            where
                ap = (cx+dx,cy)

                cont [] = ds
                cont dxs' = SVG.HorizontalTo SVG.OriginRelative dxs' : ds

        renderPathCommands firstp (cx, _) _ (SVG.VerticalTo SVG.OriginAbsolute (py:pys):ds)
            = LineTo ap : renderPathCommands firstp ap Nothing (cont pys)
            where
                ap = (cx,py)

                cont [] = ds
                cont pys' = SVG.VerticalTo SVG.OriginAbsolute pys' : ds

        renderPathCommands firstp (cx, cy) _ (SVG.VerticalTo SVG.OriginRelative (dy:dys):ds)
            = LineTo ap : renderPathCommands firstp ap Nothing (cont dys)
            where
                ap = (cx,cy+dy)

                cont [] = ds
                cont dys' = SVG.VerticalTo SVG.OriginRelative dys' : ds

        renderPathCommands firstp currentp _ (SVG.CurveTo origin ((c1,c2,p):ps):ds)
            = BezierTo ac1 ac2 ap : renderPathCommands firstp ap (Just ac2) (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)
                ac1 = toAbsolute currentp origin (fromRPoint c1)
                ac2 = toAbsolute currentp origin (fromRPoint c2)

                cont [] = ds
                cont ps' = SVG.CurveTo origin ps' : ds

        renderPathCommands firstp currentp mbControlp (SVG.SmoothCurveTo origin ((c2,p):ps):ds)
            = BezierTo ac1 ac2 ap : renderPathCommands firstp ap (Just ac2) (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)
                ac1 = maybe ac2 (mirrorControlPoint currentp) mbControlp
                ac2 = toAbsolute currentp origin (fromRPoint c2)

                cont [] = ds
                cont ps' = SVG.SmoothCurveTo origin ps' : ds

        renderPathCommands firstp currentp _ (SVG.QuadraticBezier origin ((c1,p):ps):ds)
            = cbezier : renderPathCommands firstp ap (Just ac1) (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)
                ac1 = toAbsolute currentp origin (fromRPoint c1)

                cbezier = bezierQ2C currentp ac1 ap

                cont [] = ds
                cont ps' = SVG.QuadraticBezier origin ps' : ds

        renderPathCommands firstp currentp mbControlp (SVG.SmoothQuadraticBezierCurveTo origin (p:ps):ds)
            = cbezier : renderPathCommands firstp ap (Just ac1) (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)
                ac1 = maybe currentp (mirrorControlPoint currentp) mbControlp

                cbezier = bezierQ2C currentp ac1 ap

                cont [] = ds
                cont ps' = SVG.SmoothQuadraticBezierCurveTo origin ps' : ds

        renderPathCommands firstp currentp _ (SVG.EllipticalArc origin ((rx,ry,rot,largeArcFlag,sweepFlag,p):ps):ds)
            = convertSvgArc currentp rx ry rot largeArcFlag sweepFlag ap ++ renderPathCommands firstp ap Nothing (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)

                cont [] = ds
                cont ps' = SVG.EllipticalArc origin ps' : ds

        renderPathCommands firstp@(fx,fy) (cx,cy) mbControlp (SVG.EndPath:ds)
            | fx /= cx || fy /= cy
                = LineTo firstp : renderPathCommands firstp firstp mbControlp ds
            | otherwise
                = renderPathCommands firstp firstp mbControlp ds

        renderPathCommands _ _ _ _ = []

        renderTree :: TransformationMatrix -> SVG.Tree -> [PathCommand]
        renderTree m (SVG.GroupTree g) = renderTrees (applyTransformations m (SVG._transform (SVG._groupDrawAttributes g))) (SVG._groupChildren g)
        renderTree m (SVG.PathTree p) = map (transform tr) $ renderPathCommands (0,0) (0,0) Nothing (SVG._pathDefinition p)
           where
                tr = applyTransformations m (SVG._transform (SVG._pathDrawAttributes p))

        renderTree m (SVG.RectangleTree r)
            | rx == 0.0 && ry == 0.0
                = map (transform tr) [MoveTo (x,y), LineTo (x+w,y), LineTo (x+w,y+h), LineTo (x,y+h), LineTo (x,y)]
            | otherwise
                = map (transform tr)
                      ([MoveTo (x,y+ry)]     ++ convertSvgArc (x,y+ry) rx ry 0 False True (x+rx, y) ++
                       [LineTo (x+w-rx,y)]   ++ convertSvgArc (x+w-rx,y) rx ry 0 False True (x+w, y+ry) ++
                       [LineTo (x+w,y+h-ry)] ++ convertSvgArc (x+w,y+h-ry) rx ry 0 False True (x+w-rx, y+h) ++
                       [LineTo (x+rx,y+h)]   ++ convertSvgArc (x+rx, y+h) rx ry 0 False True (x, y+h-ry) ++
                       [LineTo (x,y+ry)])
            where
                (x,y) = fromSvgPoint dpi (SVG._rectUpperLeftCorner r)
                w = fromSvgNumber dpi (SVG._rectWidth r)
                h = fromSvgNumber dpi (SVG._rectHeight r)
                (rx, ry) = mapTuple (fromSvgNumber dpi) (SVG._rectCornerRadius r)
                tr = applyTransformations m (SVG._transform (SVG._rectDrawAttributes r))

        renderTree m (SVG.LineTree l) = [MoveTo p1, LineTo p2]
            where
                p1 = transform tr (fromSvgPoint dpi (SVG._linePoint1 l))
                p2 = transform tr (fromSvgPoint dpi (SVG._linePoint2 l))
                tr = applyTransformations m (SVG._transform (SVG._lineDrawAttributes l))

        renderTree m (SVG.PolyLineTree l) = map (transform tr) (MoveTo p0:map LineTo ps)
            where
                (p0:ps) = map (\(Linear.V2 x y) -> (x,y)) (SVG._polyLinePoints l)
                tr = applyTransformations m (SVG._transform (SVG._polyLineDrawAttributes l))

        renderTree m (SVG.PolygonTree l) = map (transform tr) (MoveTo p0:map LineTo (ps ++ [p0]))
            where
                (p0:ps) = map (\(Linear.V2 x y) -> (x,y)) (SVG._polygonPoints l)
                tr = applyTransformations m (SVG._transform (SVG._polygonDrawAttributes l))

        renderTree m (SVG.EllipseTree e) = map (transform tr) (MoveTo (cx-rx,cy) : bs1++bs2++bs3++bs4)
            where
                bs1 = convertSvgArc (cx-rx, cy) rx ry 0 False True (cx, cy-ry)
                bs2 = convertSvgArc (cx, cy-ry) rx ry 0 False True (cx+rx, cy)
                bs3 = convertSvgArc (cx+rx, cy) rx ry 0 False True (cx, cy+ry)
                bs4 = convertSvgArc (cx, cy+ry) rx ry 0 False True (cx-rx, cy)

                (cx,cy) = fromSvgPoint dpi (SVG._ellipseCenter e)
                rx = fromSvgNumber dpi (SVG._ellipseXRadius e)
                ry = fromSvgNumber dpi (SVG._ellipseYRadius e)
                tr = applyTransformations m (SVG._transform (SVG._ellipseDrawAttributes e))

        renderTree m (SVG.CircleTree c) = map (transform tr) (MoveTo (cx-r,cy) : bs1++bs2++bs3++bs4)
            where
                bs1 = convertSvgArc (cx-r, cy) r r 0 False True (cx, cy-r)
                bs2 = convertSvgArc (cx, cy-r) r r 0 False True (cx+r, cy)
                bs3 = convertSvgArc (cx+r, cy) r r 0 False True (cx, cy+r)
                bs4 = convertSvgArc (cx, cy+r) r r 0 False True (cx-r, cy)

                (cx,cy) = fromSvgPoint dpi (SVG._circleCenter c)
                r = fromSvgNumber dpi (SVG._circleRadius c)
                tr = applyTransformations m (SVG._transform (SVG._circleDrawAttributes c))

        {- The rest: None, UseTree, SymbolTree, TextTree, ImageTree -}
        renderTree _ _ = []

        renderTrees :: TransformationMatrix -> [SVG.Tree] -> [PathCommand]
        renderTrees m es = concatMap (renderTree m) es
