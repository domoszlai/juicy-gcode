## Synopsis

Haskell SVG to G-code converter. It aims to support almost all of the SVG features. The flavor of the generated G-Code can be configured providing a configuration file.

## Installation and usage

* Install the latest [Haskell Platform](https://www.haskell.org/platform/) if you do not have it yet
* `$ git clone https://github.com/domoszlai/juicy-gcode.git`
* `$ cabal install juicy-gcode/juicy-gcode.cabal`
* `$ juicy-gcode --help`

```
juicy-gcode - The SVG to G-Code converter

Usage: juicy-gcode.exe SVGFILE [-f|--flavor CONFIGFILE] [-o|--output OUTPUTFILE]
                       [-d|--dpi DPI]
  Convert SVGFILE to G-Code

Available options:
  -h,--help                Show this help text
  SVGFILE                  The SVG file to be converted
  -f,--flavor CONFIGFILE   Configuration of G-Code flavor
  -o,--output OUTPUTFILE   The output G-Code file (default is standard output)
  -d,--dpi DPI             Density of the SVG file (default is 72 DPI)
```

## Configuration 

The default G-Code flavor configuration file is the following:

```
gcode
{
   begin = "G17;G90;G0 Z10;G0 X0 Y0;M3;G4 P2000.000000"
   end = "G0 Z10;M5;M2" 
   toolon =  "G00 Z10"
   tooloff = "G01 Z0 F10.00"
}
```

A new configuration file can be set by the `--flavor` or `-f` command line option. 

Another configurable property is the resolution of the SVG image in DPI (dot per inch). It can be given by the `--dpi` or `-d` command line option. Default value is 72 DPI.

## Limitations

Missing features:
* text (easy with e.g. [FontyFruity](https://hackage.haskell.org/package/FontyFruity), maybe once, you can convert text to curves easily anyway)
* filling (moderately difficult)
* clipping (probably not easy, maybe once)
* images (not planned)

## Implementation

SVG images are built using the following shapes (all of these are subject of an arbitrary affine transformation):

* lines
* circles
* ellipses
* elliptic arcs with optional x axis rotation
* quadratic and cubic bezier curves


In contrast G-Code implements only

* lines
* non-elliptic arcs

That means that only lines, circles and some arcs (non-elliptic ones without rotation) can be transleted to G-Code directly. If transformations are also counted, then
only lines can be translated to G-Code directly as circles are not invariant under affine transformations. Because of this, the converter is implemented in two stages.

### Stage 1

All the SVG drawing operations are translated to a list of MoveTo, LineTo and CubicBezierTo operations as these are invariant under affine transformations.
Arcs, circles and ellipses can be easily approximated with bezier curves with a small error.

### Stage 2

Cubic bezier curves are approximated with [Biarcs](https://en.wikipedia.org/wiki/Biarc) using the algorithm described in [[1](http://www.itc.ktu.lt/index.php/ITC/article/view/11812)] and explained [here](http://dlacko.blogspot.nl/2016/10/approximating-bezier-curves-by-biarcs.html).

