module SvgArcSegment ( 
                       convertSvgArc
                     ) where

import Types                     
                
radiansPerDegree :: Double     
radiansPerDegree = pi / 180.0

calculateVectorAngle :: Double -> Double -> Double -> Double -> Double
calculateVectorAngle ux uy vx vy
    | tb >= ta
        = tb - ta
    | otherwise
        = pi * 2 - (ta - tb)
    where
        ta = atan2 uy ux
        tb = atan2 vy vx
        
-- ported from: https://github.com/vvvv/SVG/blob/master/Source/Paths/SvgArcSegment.cs
convertSvgArc :: Point -> Double -> Double -> Double -> Bool -> Bool -> Point -> [DrawOp]
convertSvgArc (x0,y0) radiusX radiusY angle largeArcFlag sweepFlag (x,y)
    | x0 == x && y0 == y
        = []
    | radiusX == 0.0 && radiusY == 0.0
        = [DLineTo (x,y)]
    | otherwise 
        = calcSegments x0 y0 theta1' segments'
    where
        sinPhi = sin (angle * radiansPerDegree)
        cosPhi = cos (angle * radiansPerDegree)

        x1dash = cosPhi * (x0 - x) / 2.0 + sinPhi * (y0 - y) / 2.0
        y1dash = -sinPhi * (x0 - x) / 2.0 + cosPhi * (y0 - y) / 2.0

        numerator = radiusX * radiusX * radiusY * radiusY - radiusX * radiusX * y1dash * y1dash - radiusY * radiusY * x1dash * x1dash

        s = sqrt(1.0 - numerator / (radiusX * radiusX * radiusY * radiusY))
        rx   = if' (numerator < 0.0) (radiusX * s) radiusX
        ry   = if' (numerator < 0.0) (radiusY * s) radiusY
        root = if' (numerator < 0.0) 
                   (0.0) 
                   ((if' ((largeArcFlag && sweepFlag) || (not largeArcFlag && not sweepFlag)) (-1.0) 1.0) * 
                        sqrt(numerator / (radiusX * radiusX * y1dash * y1dash + radiusY * radiusY * x1dash * x1dash)))
  
        cxdash = root * rx * y1dash / ry
        cydash = -root * ry * x1dash / rx

        cx = cosPhi * cxdash - sinPhi * cydash + (x0 + x) / 2.0
        cy = sinPhi * cxdash + cosPhi * cydash + (y0 + y) / 2.0
        
        theta1'  = calculateVectorAngle 1.0 0.0 ((x1dash - cxdash) / rx) ((y1dash - cydash) / ry)
        dtheta' = calculateVectorAngle ((x1dash - cxdash) / rx) ((y1dash - cydash) / ry) ((-x1dash - cxdash) / rx) ((-y1dash - cydash) / ry)
        dtheta  = if' (not sweepFlag && dtheta' > 0) 
                      (dtheta' - 2 * pi)
                      (if' (sweepFlag && dtheta' < 0) (dtheta' + 2 * pi) dtheta')
  
        segments' = ceiling (abs (dtheta / (pi / 2.0)))
        delta = dtheta / fromInteger segments'
        t = 8.0 / 3.0 * sin(delta / 4.0) * sin(delta / 4.0) / sin(delta / 2.0)
  
        calcSegments startX startY theta1 segments 
            | segments == 0
                = []
            | otherwise
                = (DBezierTo (startX + dx1, startY + dy1) (endpointX + dxe, endpointY + dye) (endpointX, endpointY) : calcSegments endpointX endpointY theta2 (segments - 1))
            where
                cosTheta1 = cos theta1
                sinTheta1 = sin theta1
                theta2 = theta1 + delta
                cosTheta2 = cos theta2
                sinTheta2 = sin theta2

                endpointX = cosPhi * rx * cosTheta2 - sinPhi * ry * sinTheta2 + cx
                endpointY = sinPhi * rx * cosTheta2 + cosPhi * ry * sinTheta2 + cy

                dx1 = t * (-cosPhi * rx * sinTheta1 - sinPhi * ry * cosTheta1)
                dy1 = t * (-sinPhi * rx * sinTheta1 + cosPhi * ry * cosTheta1)

                dxe = t * (cosPhi * rx * sinTheta2 + sinPhi * ry * cosTheta2)
                dye = t * (sinPhi * rx * sinTheta2 - cosPhi * ry * cosTheta2)

  