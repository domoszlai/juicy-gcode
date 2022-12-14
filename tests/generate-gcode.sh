#!/bin/bash
TEST_SVGS="../resources/tests/*.svg"
for svg in $TEST_SVGS
do
  echo -n "Processing $svg file... "
  juicy-gcode -o ${svg%.svg}.gcode $svg
  juicy-gcode -c linear -o ${svg%.svg}.linear.gcode $svg
  echo "ok" 
done