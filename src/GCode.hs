{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

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
import Data.Char (isSpace)

data GCodeColorBehavior = GCodeColorBehavior {
     before     :: Maybe String
   , passes     :: Maybe Int
   , parameters :: Maybe (HashMap String Float)
} deriving (Show, Generic)

instance FromJSON GCodeColorBehavior

data GCodeFlavor = GCodeFlavor {
      begin   :: Maybe String
    , end     :: Maybe String
    , toolon  :: Maybe String
    , tooloff :: Maybe String
    , colors  :: Maybe (HashMap String GCodeColorBehavior)
} deriving (Show, Generic)

instance FromJSON GCodeFlavor

data ColorState = ColorState {
      csColor  :: String
    , csBefore :: Maybe String
    , csParams :: Maybe String
    , csPasses :: Int
}

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

buildColorState :: HashMap String GCodeColorBehavior -> String -> ColorState
buildColorState colorMap color =
    ColorState color colorBefore colorParams colorPasses
    where
        colorBefore = fmap trim $ HashMap.lookup color colorMap >>= before
        colorPasses = fromMaybe 1 $ HashMap.lookup color colorMap >>= passes
        colorParams = case fromMaybe [] (HashMap.lookup color colorMap >>= parameters >>= (Just . HashMap.toList)) of
            [] -> Nothing
            params -> Just $ unwords $ map (\(w,p) -> w ++ show p) params

defaultFlavor :: GCodeFlavor
defaultFlavor =  GCodeFlavor (Just "G17\nG90\nG0 Z1\nG0 X0 Y0") (Just "G0 Z1") (Just "G01 Z0 F10.00") (Just "G00 Z1") Nothing

toString :: Maybe GCodeFlavor -> Int -> [ColoredPath] -> String
toString mbConfig dpi cps
    = maybe [] ((++ "\n").trim) (begin config) ++
      intercalate "\n" (toString' (flatten cps) (0,0) True Nothing) ++
      "\n" ++
      maybe [] ((++ "\n").trim) (end config)
    where
        config = fromMaybe defaultFlavor mbConfig

        colorMap :: HashMap String GCodeColorBehavior
        colorMap = fromMaybe HashMap.empty (colors config)

        augmentCommand mbColor cmd = maybe cmd (\params -> cmd ++ " " ++ params) (mbColor >>= csParams)

        mbToolonCommands = fmap trim (toolon config)
        mbTooloffCommands = fmap trim (tooloff config)

        toColorString Nothing = "000000"
        toColorString (Just (PixelRGBA8 r g b _)) = concatMap (printf "%02X") [r, g, b]

        flatten = concatMap (\(ColoredPath color path) -> Left (toColorString color): map Right path)

        dd :: Double
        dd = fromIntegral dpi

        mm :: Double -> Double
        mm px = (px * 2.54 * 10) / dd

        toString' :: [Either String PathCommand] -> (Double, Double) -> Bool -> Maybe ColorState -> [String]

        -- Move, tool off
        toString' (Right (MoveTo p@(x,y)) : gs) _ False mbCurrentColor
            = printf "G00 X%.4f Y%.4f" (mm x) (mm y) :
              toString' gs p False mbCurrentColor

        -- Switch off tool before move
        toString' (Right (MoveTo p@(x,y)) : gs) _ True mbCurrentColor
            = let cont = printf "G00 X%.4f Y%.4f" (mm x) (mm y) : toString' gs p False mbCurrentColor
               in case mbTooloffCommands of
                    Nothing -> cont
                    Just tooloffCommands -> tooloffCommands : cont

        -- Switch on tool before drawing
        toString' gs cp False mbCurrentColor
            = let cont = toString' gs cp True mbCurrentColor
               in case mbToolonCommands of
                    Nothing -> cont
                    Just toolonCommands -> toolonCommands : cont

        -- Drawing line
        toString' (Right (LineTo p@(x,y)) : gs) _ True mbCurrentColor
            = augmentCommand mbCurrentColor (printf "G01 X%.4f Y%.4f" (mm x) (mm y)) :
              toString' gs p True mbCurrentColor

        -- Drawing arc
        toString' (Right (ArcTo (ox,oy) p@(x,y) cw) : gs) (cx,cy) True mbCurrentColor
            = arcStr : toString' gs p True mbCurrentColor
            where
                i = ox - cx
                j = oy - cy

                cmd = if' cw "G03" "G02"

                arcStr = augmentCommand mbCurrentColor $
                         printf "%s X%.4f Y%.4f I%.4f J%.4f" cmd (mm x) (mm y) (mm i) (mm j)

        -- Drawing bezier
        toString' (Right (BezierTo (c1x,c1y) (c2x,c2y) p2@(p2x,p2y)) : gs) (p1x,p1y) True mbCurrentColor
            = bStr : toString' gs p2 True mbCurrentColor
            where
                i = c1x - p1x
                j = c1y - p1y
                p = c2x - p2x
                q = c2y - p2y

                bStr = augmentCommand mbCurrentColor $
                       printf "G05 I%.4f J%.4f P%.4f Q%.4f X%.4f Y%.4f" (mm i) (mm j) (mm p) (mm q) (mm p2x) (mm p2y)

        -- Potential color change, no current color
        toString' (Left newColor : gs) p drawing mbCurrentColor
            | fmap csColor mbCurrentColor == Just newColor
                =  toString' gs p drawing mbCurrentColor
            | otherwise
                = let cont = toString' gs p drawing (Just newColorState)
                   in case csBefore newColorState of
                        Nothing -> cont
                        Just beforeCmd -> beforeCmd : cont
            where
                newColorState = buildColorState colorMap newColor

        toString' [] _ _ _ = []
