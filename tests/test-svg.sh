#!/bin/bash
TEST_SVGS="../resources/tests/*.svg"
for svg in $TEST_SVGS
do
  echo -n "Testing $svg file... "
  juicy-gcode.exe $svg | cmp -s ${svg%.svg}.gcode -
  if [ $? -eq 0 ]
  then
   echo "ok"
  else
   echo "failed"
   exit 1 
  fi  
done

exit 0