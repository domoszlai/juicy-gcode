# Revision history for juicy-gcode

## 0.2.0.2 -- 2022-10-31

- Fix a problem triggered by non-quadratic inflexion point equations

## 0.2.0.1 -- 2020-08-24

- Breaking change: change default DPI to 96 instead of 72
- Breaking change: the option to mirror the Y axis is removed (it is always mirrored now for correct result)
- Add --version flag

## 0.1.0.10 -- 2020-08-19

- Improve algorithmic stability at small details
- Fix issue with SVG Line element

## 0.1.0.9 -- 2020-05-27

- Add option to generate bezier curves instead of arcs

## 0.1.0.8 -- 2020-05-19

- Fix unhandled bezier edge cases resulting NaNs in GCode

## 0.1.0.7 -- 2020-05-15

- Add support for the viewBox attribute

## 0.1.0.6 -- 2020-05-11

- Add option to mirror Y axis

## 0.1.0.5.2 -- 2020-04-11

- Update dependencies

## 0.1.0.5.1 -- 2018-08-08

- Update documentation

## 0.1.0.5 -- 2018-08-08

- Simplify special bezier curves to lines

## 0.1.0.4 -- 2017-12-30

- Update LICENSE

## 0.1.0.3 -- 2017-03-19

- Fix typo in cabal file

## 0.1.0.2 -- 2017-03-18

- Fix generating arcs with negative I or J

## 0.1.0.1 -- 2016-10-31

- Minor changes to the package description and README.

## 0.1.0.0 -- 2016-10-30

- First version. Mostly feature complete, but not well tested.
