module GCode ( GCodeFlavor(..)
             , defaultFlavor
             , toString
             ) where

import Data.List             
import Text.Printf

import Types

data GCodeFlavor = GCodeFlavor { begin   :: String
                             , end     :: String
                             , toolon  :: String
                             , tooloff :: String
                             }

defaultFlavor =  GCodeFlavor "G17\nG90\nG0 Z10\nG0 X0 Y0\nM3\nG4 P2000.000000" "G0 Z10\nM5\nM2" "G01 Z0 F10.00" "G00 Z10"

toString :: GCodeFlavor -> Int -> [GCodeOp] -> String
toString (GCodeFlavor begin end on off) dpi gs = begin ++ "\n" ++ intercalate "\n" (toString' gs (0,0) True) ++ "\n" ++ end
    where
        dd :: Double
        dd = fromIntegral dpi
    
        mm :: Double -> Double
        mm px = (px / dd) * 2.54 * 10 
    
        toString' (GMoveTo p@(x,y) : gs) _ False = printf "G00 X%.4f Y%.4f" (mm x) (mm y) : toString' gs p False
        toString' (GMoveTo p@(x,y) : gs) _ True = off : printf "G00 X%.4f Y%.4f" (mm x) (mm y) : toString' gs p False
        toString' gs cp False = on : toString' gs cp True
        toString' (GLineTo p@(x,y) : gs) cp True = printf "G01 X%.4f Y%.4f" (mm x) (mm y) : toString' gs p True
        toString' (GArcTo (ox,oy) p@(x,y) cw : gs) (cx,cy) True = arcStr : toString' gs p True
            where
                i = ox - cx
                j = oy - cy               
            
                cmd = if' cw "G03" "G02" 
            
                arcStr 
                    | (mm i) < 1 || (mm i) < 1
                        = printf "G01 X%.4f Y%.4f" (mm x) (mm y)
                    | otherwise 
                        = printf "%s X%.4f Y%.4f I%.4f J%.4f" cmd (mm x) (mm y) (mm i) (mm j)
                        
        toString' [] _ _ = []             