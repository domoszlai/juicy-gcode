#!/bin/bash
TEST_SVGS="../resources/tests/*.svg"
for svg in $TEST_SVGS
do
  echo "Processing $svg file..."
  juicy-gcode -o ${svg%.svg}.gcode $svg 
done