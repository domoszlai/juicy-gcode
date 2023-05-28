# Juicy-gcode: A lightweight 2.5D SVG to GCode converter for maximal curve fitting

[![Hackage](https://img.shields.io/hackage/v/juicy-gcode.svg)](https://hackage.haskell.org/package/juicy-gcode)
[![Appveyor](https://ci.appveyor.com/api/projects/status/github/domoszlai/juicy-gcode?branch=master&svg=true)](https://ci.appveyor.com/project/domoszlai/juicy-gcode)

## Overview

Juicy-GCode is a command-line application that converts SVG files to GCode. It provides flexible options for curve approximation and allows configurable GCode generation based on color information.

## Features 

- **Configurable Curve Approximation**: Juicy-GCode offers three approximation methods to suit your needs
   - with [biarcs](http://dlacko.org/blog/2016/10/19/approximating-bezier-curves-by-biarcs/) for maximal curve fitting: bezier curves are approximated with arcs ([G2, G3 commands](https://marlinfw.org/meta/gcode/)), the resulting path is [G1 continuous](https://skill-lync.com/blogs/introductions-to-surface-continuities-and-its-types)
   - with linear approximation when arcs are not supported by your firmware: bezier curves are approximated with line segments ([G0, G1 commands](https://marlinfw.org/meta/gcode/)), the resulting path is [not smooth](https://skill-lync.com/blogs/introductions-to-surface-continuities-and-its-types), and the generated GCode is usually significantly larger 
   - with cubic bezier curves if your firmware supports it: some firmwares (e.g. [Marlin](https://marlinfw.org/docs/gcode/G005.html)) can handle bezier curves directly ([G5 command](https://marlinfw.org/meta/gcode/)), the result is [G2 continuous](https://skill-lync.com/blogs/introductions-to-surface-continuities-and-its-types)
- **Configurable 2.5D GCode generation**: Juicy-GCode allows you to configure the GCode generation by color information (this feature enables e.g. carving of 3D objects based on a single SVG file)
   - extra GCode can be added to the beginning of a continous colored path
   - GCode [parameters](https://reprap.org/wiki/G-code#G0_.26_G1:_Move) e.g. `E`, `F` or `S` can be set per color
   - the number of passes can be set for a given color (upcoming feature)

## Installation

The easiest way is to download one of the prebuilt binaries from the [releases page](https://github.com/domoszlai/juicy-gcode/releases).
Alternatively, you can build from source code as follows:

- Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) if you do not have it yet
- `$ git clone https://github.com/domoszlai/juicy-gcode.git`
- `$ stack build`
- `$ stack install`
- `$ juicy-gcode --help`

## Usage

> :warning: **Breaking change**: Since version 1.0.0.0, the flavor file format changed to YAML. If you use a pre 1.0 version, please refer to [this historical README file](https://github.com/domoszlai/juicy-gcode/blob/9d573918eb1c4a99801c8d6745f7471ba987828c/README.md)

> :warning: **Breaking change**: Since version 0.3.0.0, `--generate-bezier` has been removed in favor of the more generic `--curve-fitting` parameter and `--resolution` has been renamed to `--tolerance`

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
* *--tolerance* (default is 0.1 mm) maximum allowed deviation of the approximation curve
 
```
$ juicy-gcode SVGFILE --dpi 72 --tolerance 0.01 
```

Curve fitting options (default is `biarc`):

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

The generated GCode is highly dependent on the actual device it will be executed by and one might also want to generate different GCode for different colors. In `juicy-gcode` these settings are called GCode *flavor* and consist of the following:

- Begin GCode routine (commands that are executed *before* the actual print job)
- End GCode routine (commands that are executed *after* the actual print job)
- Tool on (commands to switch the tool on, e.g. lower the pen)
- Tool off (commands to switch the tool off e.g. lift the pen)
- Color dependent settings

These settings can be provided by a YAML configuration file. The default settings
are made for being able to test the generated GCode in an emulator e.g. with [LaserWeb](https://laserweb.yurl.ch/)
or [my hanging plotter simulator](https://github.com/domoszlai/hanging-plotter-simulator). 

```YAML
begin: |
  G17
  G90
  G0 Z1
  G0 X0 Y0
end: |
  G0 Z1
toolon: |
  G01 Z0 F10.00
tooloff: |
  G00 Z1
```

By providing your own configuration file, you can also specify color dependent settings. As an example, the following
configuration adds extra GCode configuration for the color codes `#000000` and `#FF0000`.

```YAML
colors:
   "000000": 
      before: |
        ; black
      passes: 1
      parameters:
        F: 1.0 
        S: 80
   "FF0000": 
      before: |
        ; red
      passes: 1
      parameters:
        F: 2.0 
        S: 25
```

In this example, GCode comments will be generated before any continous red or black path, the `F` and `S` GCode parameters are
set different for red and black paths (and not set for any other colors) and the number of passes are set to 1 for both colors (passes is an upcoming feature, currently this option is ignored)

Use the `-f` option to pass the GCode flavor to `juicy-gcode`:

```
$ juicy-gcode SVGFILE -f FLAVORFILE
```

## Limitations

SVG features that are not supported:

- texts
- filling
- clipping
- images
