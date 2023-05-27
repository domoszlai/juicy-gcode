#!/bin/bash

TEST_SVGS="../resources/tests/*.svg"
for svg in $TEST_SVGS
do
  echo -n "Processing $svg file... "
  juicy-gcode -o ${svg%.svg}.gcode $svg
  juicy-gcode -c linear -o ${svg%.svg}.linear.gcode $svg
  echo "ok" 
done

TEST_COLOR_SVGS="../resources/tests/color/*.svg"
for svg in $TEST_COLOR_SVGS
do
  echo -n "Processing $svg file... "
  juicy-gcode -f "../resources/tests/color/flavor.yml" -o ${svg%.svg}.gcode $svg
  echo "ok" 
done