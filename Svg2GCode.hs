import qualified Graphics.Svg as SVG
import qualified Graphics.Svg.CssTypes as CSS
import qualified Linear

import System.Environment

import Types
import Transformation
import SvgArcSegment

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

fromSvgPoint :: SVG.Point -> Point
fromSvgPoint (x,y) = (fromSvgNumber x, fromSvgNumber y)     

fromRPoint :: SVG.RPoint -> Point
fromRPoint (Linear.V2 x y) = (x, y)   
     
-- TODO: configurable DPI, em, percentage
fromSvgNumber :: SVG.Number -> Double
fromSvgNumber num = fromNumber' (CSS.toUserUnit 96 num)
    where
        fromNumber' (SVG.Num n) = n
          
-- current point + control point -> mirrored control point
mirrorControlPoint :: Point -> Point -> Point 
mirrorControlPoint (cx, cy) (cpx, cpy) = (cx + cx - cpx, cy + cy - cpy)        

-- convert a quadratic bezier to a cubic one
bezierQ2C :: Point -> Point -> Point -> DrawOp
bezierQ2C (qp0x, qp0y) (qp1x, qp1y) (qp2x, qp2y) 
    = DBezierTo (qp0x + 2.0 / 3.0 * (qp1x - qp0x), qp0y + 2.0 / 3.0 * (qp1y - qp0y))
                (qp2x + 2.0 / 3.0 * (qp1x - qp2x), qp2y + 2.0 / 3.0 * (qp1y - qp2y))
                (qp2x, qp2y)

toAbsolute _ SVG.OriginAbsolute p = p
toAbsolute (cx,cy) SVG.OriginRelative (dx,dy) = (cx+dx, cy+dy)
     
renderPathCommands :: Point -> Point -> Maybe Point -> [SVG.PathCommand] -> [DrawOp]
renderPathCommands firstp currentp _ (SVG.MoveTo origin (p:ps):ds) 
    = DMoveTo ap : renderPathCommands ap ap Nothing (cont ps)
    where
        ap = toAbsolute currentp origin (fromRPoint p)
        
        cont [] = ds
        cont ps = SVG.MoveTo origin ps : ds
        
renderPathCommands firstp currentp _ (SVG.LineTo origin (p:ps):ds) 
    = DLineTo ap : renderPathCommands firstp ap Nothing (cont ps)
    where
        ap = toAbsolute currentp origin (fromRPoint p)

        cont [] = ds
        cont ps = SVG.LineTo origin ps : ds        
        
renderPathCommands firstp (cx, cy) _ (SVG.HorizontalTo SVG.OriginAbsolute (px:pxs):ds) 
    = DLineTo ap : renderPathCommands firstp ap Nothing (cont pxs)
    where
        ap = (px,cy)

        cont [] = ds
        cont pxs = SVG.HorizontalTo SVG.OriginAbsolute pxs : ds  

renderPathCommands firstp (cx, cy) _ (SVG.HorizontalTo SVG.OriginRelative (dx:dxs):ds) 
    = DLineTo ap : renderPathCommands firstp ap Nothing (cont dxs)
    where
        ap = (cx+dx,cy)

        cont [] = ds
        cont dxs = SVG.HorizontalTo SVG.OriginRelative dxs : ds  

renderPathCommands firstp (cx, cy) _ (SVG.VerticalTo SVG.OriginAbsolute (py:pys):ds) 
    = DLineTo ap : renderPathCommands firstp ap Nothing (cont pys)
    where
        ap = (cx,py)

        cont [] = ds
        cont pys = SVG.VerticalTo SVG.OriginAbsolute pys : ds  

renderPathCommands firstp (cx, cy) _ (SVG.VerticalTo SVG.OriginRelative (dy:dys):ds) 
    = DLineTo ap : renderPathCommands firstp ap Nothing (cont dys)
    where
        ap = (cx,cy+dy)

        cont [] = ds
        cont dys = SVG.VerticalTo SVG.OriginRelative dys : ds  
        
renderPathCommands firstp currentp _ (SVG.CurveTo origin ((c1,c2,p):ps):ds) 
    = DBezierTo ac1 ac2 ap : renderPathCommands firstp ap (Just ac2) (cont ps)
    where
        ap = toAbsolute currentp origin (fromRPoint p)
        ac1 = toAbsolute currentp origin (fromRPoint c1)
        ac2 = toAbsolute currentp origin (fromRPoint c2)
        
        cont [] = ds
        cont ps = SVG.CurveTo origin ps : ds

renderPathCommands firstp currentp mbControlp (SVG.SmoothCurveTo origin ((c2,p):ps):ds) 
    = DBezierTo ac1 ac2 ap : renderPathCommands firstp ap (Just ac2) (cont ps)
    where
        ap = toAbsolute currentp origin (fromRPoint p)
        ac1 = maybe ac2 (mirrorControlPoint currentp) mbControlp
        ac2 = toAbsolute currentp origin (fromRPoint c2)
        
        cont [] = ds
        cont ps = SVG.SmoothCurveTo origin ps : ds        
        
renderPathCommands firstp currentp _ (SVG.QuadraticBezier origin ((c1,p):ps):ds) 
    = cbezier : renderPathCommands firstp ap (Just ac1) (cont ps)
    where
        ap = toAbsolute currentp origin (fromRPoint p)
        ac1 = toAbsolute currentp origin (fromRPoint c1)

        cbezier = bezierQ2C currentp ac1 ap
        
        cont [] = ds
        cont ps = SVG.QuadraticBezier origin ps : ds

renderPathCommands firstp currentp mbControlp (SVG.SmoothQuadraticBezierCurveTo origin (p:ps):ds) 
    = cbezier : renderPathCommands firstp ap (Just ac1) (cont ps)
    where
        ap = toAbsolute currentp origin (fromRPoint p)
        ac1 = maybe currentp (mirrorControlPoint currentp) mbControlp

        cbezier = bezierQ2C currentp ac1 ap
        
        cont [] = ds
        cont ps = SVG.SmoothQuadraticBezierCurveTo origin ps : ds
        
renderPathCommands firstp currentp _ (SVG.EllipticalArc origin ((rx,ry,rot,largeArcFlag,sweepFlag,p):ps):ds) 
    = convertSvgArc currentp rx ry rot largeArcFlag sweepFlag ap ++ renderPathCommands firstp ap Nothing (cont ps)
    where
        ap = toAbsolute currentp origin (fromRPoint p)
        
        cont [] = ds
        cont ps = SVG.EllipticalArc origin ps : ds

renderPathCommands firstp@(fx,fy) (cx,cy) mbControlp (SVG.EndPath:ds)
    | fx /= cx || fy /= cy
        = DLineTo firstp : renderPathCommands firstp firstp mbControlp ds 
    | otherwise    
        = renderPathCommands firstp firstp mbControlp ds
        
renderPathCommands _ _ _ _ = []     
     
renderTree :: TransformationMatrix -> SVG.Tree -> [DrawOp]
renderTree m (SVG.GroupTree g) = renderTrees (applyTransformations m (SVG._transform (SVG._groupDrawAttributes g))) (SVG._groupChildren g)
renderTree m (SVG.PathTree p) = map (transformDrawOp tr) $ renderPathCommands (0,0) (0,0) Nothing (SVG._pathDefinition p)
   where
        tr = applyTransformations m (SVG._transform (SVG._pathDrawAttributes p))

renderTree m (SVG.RectangleTree r) 
    | rx == 0.0 && ry == 0.0
        = map (transformDrawOp tr) [DMoveTo (x,y), DLineTo (x+w,y), DLineTo (x+w,y+h), DLineTo (x,y+h), DLineTo (x,y)]
    | otherwise 
        = map (transformDrawOp tr) 
              ([DMoveTo (x,y+ry)]     ++ convertSvgArc (x,y+ry) rx ry 0 False True (x+rx, y) ++
               [DLineTo (x+w-rx,y)]   ++ convertSvgArc (x+w-rx,y) rx ry 0 False True (x+w, y+ry) ++
               [DLineTo (x+w,y+h-ry)] ++ convertSvgArc (x+w,y+h-ry) rx ry 0 False True (x+w-rx, y+h) ++
               [DLineTo (x+rx,y+h)]   ++ convertSvgArc (x+rx, y+h) rx ry 0 False True (x, y+h-ry) ++
               [DLineTo (x,y+ry)])
    where
        (x,y) = fromSvgPoint (SVG._rectUpperLeftCorner r)
        w = fromSvgNumber (SVG._rectWidth r)
        h = fromSvgNumber (SVG._rectHeight r)
        (rx, ry) = mapTuple fromSvgNumber (SVG._rectCornerRadius r)
        tr = applyTransformations m (SVG._transform (SVG._rectDrawAttributes r))    
    
renderTree m (SVG.LineTree l) = [DMoveTo p1, DLineTo p2]
    where
        p1 = transformPoint tr (fromSvgPoint (SVG._linePoint1 l))
        p2 = transformPoint tr (fromSvgPoint (SVG._linePoint1 l))
        tr = applyTransformations m (SVG._transform (SVG._lineDrawAttributes l))
     
renderTree m (SVG.PolyLineTree l) = map (transformDrawOp tr) (DMoveTo p0:map DLineTo ps) 
    where
        (p0:ps) = map (\(Linear.V2 x y) -> (x,y)) (SVG._polyLinePoints l)
        tr = applyTransformations m (SVG._transform (SVG._polyLineDrawAttributes l))
     
renderTree m (SVG.PolygonTree l) = map (transformDrawOp tr) (DMoveTo p0:map DLineTo (ps ++ [p0])) 
    where
        (p0:ps) = map (\(Linear.V2 x y) -> (x,y)) (SVG._polygonPoints l)
        tr = applyTransformations m (SVG._transform (SVG._polygonDrawAttributes l))
          
renderTree m (SVG.EllipseTree e) = map (transformDrawOp tr) (DMoveTo (cx-rx,cy) : bs1++bs2++bs3++bs4)
    where
        bs1 = convertSvgArc (cx-rx, cy) rx ry 0 False True (cx, cy-ry)
        bs2 = convertSvgArc (cx, cy-ry) rx ry 0 False True (cx+rx, cy)
        bs3 = convertSvgArc (cx+rx, cy) rx ry 0 False True (cx, cy+ry)
        bs4 = convertSvgArc (cx, cy+ry) rx ry 0 False True (cx-rx, cy)
           
        (cx,cy) = fromSvgPoint (SVG._ellipseCenter e)
        rx = fromSvgNumber (SVG._ellipseXRadius e)
        ry = fromSvgNumber (SVG._ellipseYRadius e)
        tr = applyTransformations m (SVG._transform (SVG._ellipseDrawAttributes e))

renderTree m (SVG.CircleTree c) = map (transformDrawOp tr) (DMoveTo (cx-r,cy) : bs1++bs2++bs3++bs4)
    where
        bs1 = convertSvgArc (cx-r, cy) r r 0 False True (cx, cy-r)
        bs2 = convertSvgArc (cx, cy-r) r r 0 False True (cx+r, cy)
        bs3 = convertSvgArc (cx+r, cy) r r 0 False True (cx, cy+r)
        bs4 = convertSvgArc (cx, cy+r) r r 0 False True (cx-r, cy)
           
        (cx,cy) = fromSvgPoint (SVG._circleCenter c)
        r = fromSvgNumber (SVG._circleRadius c)
        tr = applyTransformations m (SVG._transform (SVG._circleDrawAttributes c))

{- The rest: None, UseTree, SymbolTree, TextTree, ImageTree -}
renderTree _ _ = []

renderTrees :: TransformationMatrix -> [SVG.Tree] -> [DrawOp]
renderTrees m es = concat $ map (renderTree m) es

main = do
    args <- getArgs 
    case args of
        [fn] -> do 
                mbDoc <- SVG.loadSvgFile fn
                case mbDoc of
                    (Just doc) -> print (show (renderTrees identityMatrix (SVG._elements doc)))
                    otherwise  -> print "Error during opening the SVG file"
        _    -> print "Usage: svg2gcode svgfile"

