module SVGExt ( documentSize
              ) where

import qualified Graphics.Svg as SVG

-- it is a replacement of SVG.documentSize as that returns (Int,Int) causing a serious
-- precision loss in the final gcode
documentSize :: Int -> SVG.Document -> (Double, Double)
documentSize _ SVG.Document { SVG._viewBox = Just (x1, y1, x2, y2)
                            , SVG._width = Just (SVG.Percent pw)
                            , SVG._height = Just (SVG.Percent ph)
                            } =
    (dx * pw, dy * ph)
      where
        dx = abs $ x2 - x1
        dy = abs $ y2 - y1

documentSize _ SVG.Document { SVG._width = Just (SVG.Num w)
                            , SVG._height = Just (SVG.Num h) } = (w, h)

documentSize dpi doc@(SVG.Document { SVG._width = Just w
                                   , SVG._height = Just h }) =
    documentSize dpi $ doc
        { SVG._width = Just $ SVG.toUserUnit dpi w
        , SVG._height = Just $ SVG.toUserUnit dpi h }

documentSize _ SVG.Document { SVG._viewBox = Just (x1, y1, x2, y2) } =
    (abs $ x2 - x1, abs $ y2 - y1)

documentSize _ _ = (1, 1)
