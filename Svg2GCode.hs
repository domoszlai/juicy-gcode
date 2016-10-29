import qualified Graphics.Svg as SVG

import Data.Monoid

import System.Environment
import Options.Applicative

import Types
import Render
import GCode
                                                 
data Options = Options { svgfile :: String
                       , cfgfile :: Maybe String
                       , outfile :: Maybe String
                       , dpi     :: Int
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
     <> value 72
     <> short 'd'
     <> metavar "DPI"     
     <> help "Density of the SVG file (default is 72 DPI)" ))

runWithOptions :: Options -> IO ()
runWithOptions (Options fn _ mbOut dpi) =
    do 
        mbDoc <- SVG.loadSvgFile fn
        case mbDoc of
            (Just doc) -> writer (toString defaultFavor dpi $ renderDoc dpi doc)
            otherwise  -> putStrLn "juicy-gcode: error during opening the SVG file"
    where
        writer = maybe putStrLn (\fn -> writeFile fn) mbOut
   
defultFavor = "[gcode]\nbegin: G17;G90;G0 Z10;G0 X0 Y0;M3;G4 P2000.000000\nend: G0 Z10;M5;M2\ntoolon: G00 Z10\ntooloff: G01 Z0 F10.00"   
   
-- readConfig :: String -> GCodefavor
-- readConfig content = 
   
main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Convert SVGFILE to G-Code" 
     <> header "juicy-gcode - The SVG to G-Code converter" )                
     