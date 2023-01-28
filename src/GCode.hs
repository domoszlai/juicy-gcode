{-# LANGUAGE DeriveGeneric #-}

module GCode ( GCodeFlavor(..)
             , toString
             ) where

import GHC.Generics

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Aeson (FromJSON (..))
import Text.Printf

import Graphics.Path
import Utils

import Codec.Picture (PixelRGBA8(..))
import Data.Maybe (fromMaybe)

data GCodeFlavor = GCodeFlavor { begin   :: Maybe [String]
                               , end     :: Maybe [String]
                               , toolon  :: Maybe [String]
                               , tooloff :: Maybe [String]
                               , colors  :: Maybe (HashMap String [String])
                               } deriving (Show, Generic)

instance FromJSON GCodeFlavor

defaultFlavor :: GCodeFlavor
defaultFlavor =  GCodeFlavor (Just ["G17","G90","G0 Z1","G0 X0 Y0"]) (Just ["G0 Z1"]) (Just ["G01 Z0 F10.00"]) (Just ["G00 Z1"]) Nothing

toString :: Maybe GCodeFlavor -> Int -> [ColoredPath] -> String
toString mbConfig dpi cps
    = intercalate "\n" (concat (begin config)) ++
      "\n" ++
      intercalate "\n" (toString' (flatten cps) (0,0) True) ++
      "\n" ++
      intercalate "\n" (concat (end config)) ++
      "\n"
    where
        config = fromMaybe defaultFlavor mbConfig
        colorMap :: HashMap String [String]
        colorMap = fromMaybe HashMap.empty (colors config)
        colorCommands color = concat $ HashMap.lookup color colorMap

        toolonCommands = concat (toolon config)
        tooloffCommands = concat (tooloff config)

        toColorString Nothing = "000000"
        toColorString (Just (PixelRGBA8 r g b _)) = concatMap (printf "%02X") [r, g, b]

        flatten = concatMap (\(ColoredPath color path) -> Left (toColorString color): map Right path)

        dd :: Double
        dd = fromIntegral dpi

        mm :: Double -> Double
        mm px = (px * 2.54 * 10) / dd

        toString' :: [Either String PathCommand] -> (Double, Double) -> Bool -> [String]
        toString' (Right (MoveTo p@(x,y)) : gs) _ False
            = printf "G00 X%.4f Y%.4f" (mm x) (mm y) : toString' gs p False
        toString' (Right (MoveTo p@(x,y)) : gs) _ True
            = tooloffCommands ++ [printf "G00 X%.4f Y%.4f" (mm x) (mm y)] ++ toString' gs p False
        toString' gs cp False
            = toolonCommands ++ toString' gs cp True
        toString' (Right (LineTo p@(x,y)) : gs) _ True
            = printf "G01 X%.4f Y%.4f" (mm x) (mm y) : toString' gs p True
        toString' (Right (ArcTo (ox,oy) p@(x,y) cw) : gs) (cx,cy) True
            = arcStr : toString' gs p True
            where
                i = ox - cx
                j = oy - cy

                cmd = if' cw "G03" "G02"

                arcStr = printf "%s X%.4f Y%.4f I%.4f J%.4f" cmd (mm x) (mm y) (mm i) (mm j)
        toString' (Right (BezierTo (c1x,c1y) (c2x,c2y) p2@(p2x,p2y)) : gs) (p1x,p1y) True
            = bStr : toString' gs p2 True
            where
                i = c1x - p1x
                j = c1y - p1y
                p = c2x - p2x
                q = c2y - p2y

                bStr = printf "G05 I%.4f J%.4f P%.4f Q%.4f X%.4f Y%.4f"
                        (mm i) (mm j) (mm p) (mm q) (mm p2x) (mm p2y)

        toString' (Left color : gs) p drawing
            = colorCommands color ++ toString' gs p drawing

        toString' [] _ _ = []
