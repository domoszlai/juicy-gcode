## Synopsis

JuicyGCode: Haskell SVG to G-code converter. It aims to support almost all of the SVG features.

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

Cubic bezier curves are approximated with [Biarcs](https://en.wikipedia.org/wiki/Biarc) using the algorithm described in [[1](http://www.itc.ktu.lt/index.php/ITC/article/view/11812)]

## Installation

* Install the latest [Haskell Platform](https://www.haskell.org/platform/)
* “`cabal update`”
* “`cabal install svg-tree matrix optparse-applicative configurator`”
* “`runghc Svg2Gcode`”

For testing Stage 1, type 
* “`cabal install rasterific`”
* “`runghc TestStage1`”

## Usage

Svg2Gcode has one obligatory parameter, the file name of the SVG file to convert.
If the file is there, it displays a list of data constructors (in haskell syntax) representing the image in Stage 1 format, and which directly can be copy-pasted into TestStage1.hs.
TestStage1.hs renders this list of drawing operations and saves the result into a file called "stage1.png" in the current directory.

## Limitations

For the time being, only Stage 1 is implemented, with the following missing features:
* text (easy with e.g. [FontyFruity](https://hackage.haskell.org/package/FontyFruity), maybe once, you can convert text to curves easily anyway)
* filling (moderately difficult)
* clipping (probably not easy, maybe once)
* images (not planned)

