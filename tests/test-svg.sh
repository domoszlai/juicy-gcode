#!/bin/bash

TEST_SVGS="../resources/tests/*.svg"
for svg in $TEST_SVGS
do
  echo -n "Testing $svg file: biarc... "
  juicy-gcode $svg | cmp -s ${svg%.svg}.gcode -
  if [ $? -eq 0 ]
  then
   echo -n "ok"
  else
   echo "failed"
   exit 1 
  fi  

  echo -n " linear... "
  juicy-gcode -c linear $svg | cmp -s ${svg%.svg}.linear.gcode -
  if [ $? -eq 0 ]
  then
   echo "ok"
  else
   echo "failed"
   exit 1 
  fi  
done

TEST_COLOR_SVGS="../resources/tests/color/*.svg"
for svg in $TEST_COLOR_SVGS
do
  echo -n "Testing $svg file: ... "
  juicy-gcode -f "../resources/tests/color/flavor.yml" $svg | cmp -s ${svg%.svg}.gcode -
  if [ $? -eq 0 ]
  then
   echo "ok"
  else
   echo "failed"
   exit 1 
  fi  
done

exit 0