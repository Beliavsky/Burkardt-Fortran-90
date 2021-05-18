#! /bin/bash
#
gfortran -c -Wall midpoint_fixed.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv midpoint_fixed.o ~/lib/midpoint_fixed.o
#
echo "Normal end of execution."
