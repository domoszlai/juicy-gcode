# Juicy-gcode: A lightweight SVG to GCode converter for maximal curve fitting

[![Hackage](https://img.shields.io/hackage/v/juicy-gcode.svg)](https://hackage.haskell.org/package/juicy-gcode)
[![Appveyor](https://ci.appveyor.com/api/projects/status/github/domoszlai/juicy-gcode?branch=master&svg=true)](https://ci.appveyor.com/project/domoszlai/juicy-gcode)

## Overview

Juicy-gcode is a configurable SVG to G-code converter that approximates bezier curves based on your needs:
- with [biarcs](http://dlacko.org/blog/2016/10/19/approximating-bezier-curves-by-biarcs/) for maximal curve fitting: bezier curves are approximated with arcs ([G2, G3 commands](https://marlinfw.org/meta/gcode/)), the resulting path is [G1 continues](https://skill-lync.com/blogs/introductions-to-surface-continuities-and-its-types)
- with linear approximation when arcs are not supported by your firmware: bezier curves are approximated with line segments ([G0, G1 commands](https://marlinfw.org/meta/gcode/)), the resulting path is [not smooth](https://skill-lync.com/blogs/introductions-to-surface-continuities-and-its-types), and the generated gcode is usually significantly larger 
- with cubic bezier curves if your firmware supports it: some firmwares (e.g. [Marlin](https://marlinfw.org/docs/gcode/G005.html)) can handle bezier curves directly ([G5 command](https://marlinfw.org/meta/gcode/)), the result is [G2 continues](https://skill-lync.com/blogs/introductions-to-surface-continuities-and-its-types)

## Installation

The easiest way is to download one of the pre-built binaries from the [releases page](https://github.com/domoszlai/juicy-gcode/releases).
Alternatively, you can build from source code as follows:

- Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) if you do not have it yet
- `$ git clone https://github.com/domoszlai/juicy-gcode.git`
- `$ stack build`
- `$ stack install`
- `$ juicy-gcode --help`

## Usage

> :warning: **Breaking change**: Since version 0.3.0.0, `--generate-bezier` has been replaced with the more generic `--curve-fitting` parameter and `--resolution` has been replaced by `--tolerance`

> :warning: **Breaking change**: Since version 0.2.0.1, default DPI is changed to 96 and the option to mirror the Y axis is removed (it is always mirrored now for correct result)

The easier way to use juicy-gcode is to simply provide an SVG file name. The generated GCode will be written to standard output. The default approximation method is the biarcs based.

```
$ juicy-gcode SVGFILE
```

Alternativly, you can provide an output file name as well.

```
$ juicy-gcode SVGFILE -o OUTPUT
```

Sometimes you want to overwrite some default settings. These are the 

* *--dpi* (default 96 DPI) [the resolution of the SVG file](https://developer.mozilla.org/en-US/docs/Web/CSS/resolution) that is used to determine the size of the SVG when it does not contain explicit units
* *--tolerance* (default is 0.1 mm) maximum allowed derivation of the approximation curve
 
```
$ juicy-gcode SVGFILE --dpi 72 --tolerance 0.01 
```

Curve fitting options:

```
$ juicy-gcode SVGFILE --curve-fitting=biarc
```
```
$ juicy-gcode SVGFILE --curve-fitting=linear
```
```
$ juicy-gcode SVGFILE --curve-fitting=cubic-bezier
```

## Configuration

The generated GCode is highly dependent on the actual device it will be executed by. In juicy-gcode these settings are called
GCode *flavor* and consists of the following:

- Begin GCode routine (commands that are executed *before* the actual print job)
- End GCode routine (commands that are executed *after* the actual print job)
- Tool on (commands to switch the tool on, e.g. lower pen)
- Tool off (commands to switch the tool off e.g. lift pen)

These settings can be provided by a configuration file. The default settings
are made for being able to test the generated GCode in an emulator e.g. with [LaserWeb](https://laserweb.yurl.ch/)
or [my hanging plotter simulator](https://github.com/domoszlai/hanging-plotter-simulator). 

```
gcode
{
   begin = "G17;G90;G0 Z1;G0 X0 Y0"
   end = "G0 Z1"
   toolon =  "G00 Z1"
   tooloff = "G01 Z0 F10.00"
}
```

In the case you want to overwrite it, copy this favor to a text file and modify it according to your need. Then use juicy-gcode as follows:

```
$ juicy-gcode SVGFILE -f FLAVORFILE
```

## Future development

Juicy-gcode was originally developed as a testbed for my hanging plotter project, but over the years
it reached maturity and became a really usuable tool. My main idea for further development is to turn it
into a tool that can drive CNCs in 2.5 dimensions (e.g. carving, engraving) with just one colored SVG file.

To be able to test and enjoy that software, I need a proper CNC. Please consider donating a small amount for that purpose,
or donate an actual CNC if you have a spare one for whatever reason.

**[Donate for a CNC](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=UGFZYDQSTF58L&source=https://github.com/domoszlai/juicy-gcode/)**

Collected so far: 229.47&euro;
Target: >= 209&euro;

Thank you so much for all people supporting the development!

## Limitations

SVG features that are not supported:

- texts
- filling
- clipping
- images
