{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use tuple-section" #-}

import qualified Graphics.Svg as SVG

import Options.Applicative
import Paths_juicy_gcode (version)
import Data.Version (showVersion)
import Development.GitRev (gitHash)

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.HashMap.Strict (fromList, toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack, unpack, replace)
import Data.List (stripPrefix)

import Render
import GCode


data Options = Options { _svgfile        :: String
                       , _cfgfile        :: Maybe String
                       , _outfile        :: Maybe String
                       , _dpi            :: Int
                       , _resolution     :: Double
                       , _approximation  :: Maybe Approximation
                       }

options :: Parser Options
options = Options
  <$> argument str
      ( metavar "SVGFILE"
     <> help "The SVG file to be converted" )
  <*> optional (strOption
      ( long "flavor"
     <> short 'f'
     <> metavar "CONFIGFILE"
     <> help "Configuration of G-Code flavor" ))
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "OUTPUTFILE"
     <> help "The output G-Code file (default is standard output)" ))
  <*> option auto
      ( long "dpi"
     <> value 96
     <> short 'd'
     <> metavar "DPI"
     <> help "Used to determine the size of the SVG when it does not contain any units; dot per inch (default is 96)" )
 <*> option auto
      ( long "tolerance"
     <> value 0.1
     <> short 't'
     <> metavar "TOLERANCE"
     <> help "Maximum derivation of the approximation curve" )
  <*> optional (option parseApproximation
      ( long "curve-fitting"
      <> short 'c'
      <> metavar "TYPE"
      <> help "Bezier curve approximation algorithm. TYPE can be linear, biarc (default) or cubic-bezier" ))

parseApproximation :: ReadM Approximation
parseApproximation = str >>= \s -> case s of
      "biarc"        -> return BiArc
      "linear"       -> return Linear
      "cubic-bezier" -> return CubicBezier
      _ -> readerError "Accepted bezier approximation types are 'biarc', 'linear', and 'cubic-bezier'."

runWithOptions :: Options -> IO ()
runWithOptions (Options svgFile mbCfg mbOut dpi resolution mbApproximation) =
    do
        mbDoc <- SVG.loadSvgFile svgFile
        flavor <- maybe (return defaultFlavor) readFlavor mbCfg
        case mbDoc of
            (Just doc) -> writer (toString flavor dpi $ renderDoc (fromMaybe BiArc mbApproximation) dpi resolution doc)
            Nothing    -> putStrLn "juicy-gcode: error during opening the SVG file"
    where
        writer = maybe putStr writeFile mbOut

toLines :: Text -> String
toLines t = unpack $ replace (pack ";") (pack "\n") t

readFlavor :: FilePath -> IO GCodeFlavor
readFlavor cfgFile = do
  cfg          <- C.load [C.Required cfgFile]
  begin        <- C.require cfg (pack "gcode.begin")
  end          <- C.require cfg (pack "gcode.end")
  toolon       <- C.require cfg (pack "gcode.toolon")
  tooloff      <- C.require cfg (pack "gcode.tooloff")
  colors       <- C.getMap cfg
  return $ GCodeFlavor
            (toLines begin)
            (toLines end)
            (toLines toolon)
            (toLines tooloff)
            ((fromList . mapMaybe convertEntry . toList) colors)
  where
    convertEntry :: (CT.Name, CT.Value) -> Maybe (String, String)
    convertEntry (key, val) = case CT.convert val of
                                  Nothing -> Nothing
                                  Just a  -> (\k -> (k, unpack a)) <$> stripPrefix "colors." (unpack key)

versionOption :: Parser (a -> a)
versionOption = infoOption
                    (concat ["juicy-gcode ", showVersion version, ", git revision ", $(gitHash)])
                    (long "version" <> short 'v' <> help "Show version")

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info (helper <*> versionOption <*> options)
      ( fullDesc
     <> progDesc "Convert SVGFILE to G-Code"
     <> header "juicy-gcode - The SVG to G-Code converter" )