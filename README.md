# Juicy-gcode: A lightweight SVG to GCode converter for maximal curve fitting

[![Hackage](https://img.shields.io/hackage/v/juicy-gcode.svg)](https://hackage.haskell.org/package/juicy-gcode)
[![Travis](https://travis-ci.org/domoszlai/juicy-gcode.svg?branch=master)](http://travis-ci.org/domoszlai/juicy-gcode)
![Appveyor](https://ci.appveyor.com/api/projects/status/github/domoszlai/juicy-gcode?branch=master&svg=true)

## Overview

Haskell SVG to G-code converter that aims to support most SVG features. The flavor of the generated G-Code can be influenced providing a configuration file.
Juicy-gcode, in contrast to most SVG to G-Code converters, approximates bezier curves with [biarcs](http://dlacko.org/blog/2016/10/19/approximating-bezier-curves-by-biarcs/) instead of line segments
that results in much better curve fit.

## Installation and usage

The easiest way is to download one of the pre-built binaries from the [releases page](https://github.com/domoszlai/juicy-gcode/releases).
Alternatively, you can build from source code as follows:

- Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) if you do not have it yet
- `$ git clone https://github.com/domoszlai/juicy-gcode.git`
- `$ stack build`
- `$ stack install`
- `$ juicy-gcode --help`

```
juicy-gcode - The SVG to G-Code converter

Usage: juicy-gcode.exe SVGFILE [-f|--flavor CONFIGFILE] [-o|--output OUTPUTFILE]
                       [-d|--dpi DPI] [-m|--mirror-y-axis]
  Convert SVGFILE to G-Code

Available options:
  -h,--help                Show this help text
  SVGFILE                  The SVG file to be converted
  -f,--flavor CONFIGFILE   Configuration of G-Code flavor
  -o,--output OUTPUTFILE   The output G-Code file (default is standard output)
  -d,--dpi DPI             Density of the SVG file (default is 72 DPI)
  -m,--mirror-y-axis       Mirror Y axis to have the result in G-Code coordinate system
  -b,--generate-bezier     Generate bezier curves (G5) instead of arcs (G2,G3)
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

- text (easy with e.g. [FontyFruity](https://hackage.haskell.org/package/FontyFruity), maybe once, you can convert text to curves easily anyway)
- filling (moderately difficult)
- clipping (probably not easy, maybe once)
- images (not planned)

## Testing and bugs

There is a JavaScript [hanging plotter simulator](https://github.com/domoszlai/hanging-plotter-simulator) mainly developed to test the generated gcode.
Please file an issue if you run into a problem (or drop me an email to dlacko @ gmail.com).

## Implementation

SVG images are built using the following shapes (all of these are subject of an arbitrary affine transformation):

- lines
- circles
- ellipses
- elliptic arcs with optional x axis rotation
- quadratic and cubic bezier curves

In contrast G-Code implements only

- lines
- non-elliptical arcs

That means that only lines, circles and some arcs (non-elliptic ones without rotation) can be translated to G-Code directly. If transformations are also counted, then
only lines can be translated to G-Code directly as circles are not invariant under affine transformations. Because of this, the converter is implemented in two stages.

### Stage 1

All the SVG drawing operations are translated to a list of MoveTo, LineTo and CubicBezierTo operations as these are invariant under affine transformations.
Arcs, circles and ellipses can be easily approximated with bezier curves with a small error.

### Stage 2

Cubic bezier curves are approximated with [Biarcs](https://en.wikipedia.org/wiki/Biarc) using the algorithm described in [[1](http://www.itc.ktu.lt/index.php/ITC/article/view/11812)] and explained [here](http://dlacko.org/blog/2016/10/19/approximating-bezier-curves-by-biarcs/).
