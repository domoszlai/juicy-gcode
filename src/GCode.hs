module GCode ( GCodeFlavor(..)
             , defaultFlavor
             , toString
             ) where

import Data.List
import Text.Printf

import Types

data GCodeFlavor = GCodeFlavor { _begin   :: String
                               , _end     :: String
                               , _toolon  :: String
                               , _tooloff :: String
                               }

defaultFlavor :: GCodeFlavor
defaultFlavor =  GCodeFlavor "G17\nG90\nG0 Z1\nG0 X0 Y0" "G0 Z1" "G01 Z0 F10.00" "G00 Z1"

toString :: GCodeFlavor -> Int -> [GCodeOp] -> String
toString (GCodeFlavor begin end on off) dpi gops 
    = begin ++
      "\n" ++ 
      intercalate "\n" (toString' gops (0,0) True) ++ 
      "\n" ++ 
      end ++
      "\n"
    where
        dd :: Double
        dd = fromIntegral dpi

        mm :: Double -> Double
        mm px = (px * 2.54 * 10) / dd

        toString' (GMoveTo p@(x,y) : gs) _ False
            = printf "G00 X%.4f Y%.4f" (mm x) (mm y) : toString' gs p False
        toString' (GMoveTo p@(x,y) : gs) _ True
            = off : printf "G00 X%.4f Y%.4f" (mm x) (mm y) : toString' gs p False
        toString' gs cp False
            = on : toString' gs cp True
        toString' (GLineTo p@(x,y) : gs) _ True
            = printf "G01 X%.4f Y%.4f" (mm x) (mm y) : toString' gs p True
        toString' (GArcTo (ox,oy) p@(x,y) cw : gs) (cx,cy) True
            = arcStr : toString' gs p True
            where
                i = ox - cx
                j = oy - cy

                cmd = if' cw "G03" "G02"

                arcStr = printf "%s X%.4f Y%.4f I%.4f J%.4f" cmd (mm x) (mm y) (mm i) (mm j)
        toString' (GBezierTo (c1x,c1y) (c2x,c2y) p2@(p2x,p2y) : gs) (p1x,p1y) True
            = bStr : toString' gs p2 True
            where
                i = c1x - p1x
                j = c1y - p1y
                p = c2x - p2x
                q = c2y - p2y

                bStr = printf "G05 I%.4f J%.4f P%.4f Q%.4f X%.4f Y%.4f"
                        (mm i) (mm j) (mm p) (mm q) (mm p2x) (mm p2y)

        toString' [] _ _ = []
