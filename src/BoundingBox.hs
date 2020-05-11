module BoundingBox ( BoundingBox
                   , boundingBox
                   ) where

import Types

type BoundingBox = (Point, Point)

points :: DrawOp -> [Point]
points (DMoveTo p) = [p]
points (DLineTo p) = [p]
points (DBezierTo p1 p2 p3) = [p1, p2, p3]

extendBB :: BoundingBox -> Point -> BoundingBox
extendBB ((topLeftX, topLeftRight), (bottomRightX, bottomRightY)) (x,y) =
    ((min topLeftX x, min topLeftRight y), (max bottomRightX x,max bottomRightY y))

boundingBox  :: [DrawOp] -> BoundingBox
boundingBox drawOps = 
    foldl extendBB ((0,0),(0,0)) (concatMap points drawOps)


