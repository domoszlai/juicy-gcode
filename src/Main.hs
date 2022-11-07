{-# LANGUAGE TemplateHaskell #-}

import qualified Graphics.Svg as SVG

import Options.Applicative
import Paths_juicy_gcode (version)
import Data.Version (showVersion)
import Development.GitRev (gitHash)

import Data.Text (Text, pack, unpack, replace)
import qualified Data.Configurator as C
import Data.Maybe

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
  <*> (optional $ strOption
      ( long "flavor"
     <> short 'f'
     <> metavar "CONFIGFILE"
     <> help "Configuration of G-Code flavor" ))
  <*> (optional $ strOption
      ( long "output"
     <> short 'o'
     <> metavar "OUTPUTFILE"
     <> help "The output G-Code file (default is standard output)" ))
  <*> (option auto
      ( long "dpi"
     <> value 96
     <> short 'd'
     <> metavar "DPI"
     <> help "Used to determine the size of the SVG when it does not contain any units; dot per inch (default is 96)" ))
 <*> (option auto
      ( long "resolution"
     <> value 0.1
     <> short 'r'
     <> metavar "RESOLUTION"
     <> help "Shorter paths are replaced by line segments; mm (default is 0.1)" ))
  <*> (optional $ (option parseApproximation
      ( long "bezier-approximation"
      <> short 'a'
      <> metavar "TYPE"
      <> help "Configuration of G-Code flavor" )))

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
        writer = maybe putStr (\fn -> writeFile fn) mbOut

toLines :: Text -> String
toLines t = unpack $ replace (pack ";") (pack "\n") t

readFlavor :: FilePath -> IO GCodeFlavor
readFlavor cfgFile = do
  cfg          <- C.load [C.Required cfgFile]
  begin        <- C.require cfg (pack "gcode.begin")
  end          <- C.require cfg (pack "gcode.end")
  toolon       <- C.require cfg (pack "gcode.toolon")
  tooloff      <- C.require cfg (pack "gcode.tooloff")
  return $ GCodeFlavor (toLines begin) (toLines end) (toLines toolon) (toLines tooloff)

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