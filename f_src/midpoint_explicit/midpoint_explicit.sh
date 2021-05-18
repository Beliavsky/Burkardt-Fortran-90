#! /bin/bash
#
gfortran -c -Wall midpoint_explicit.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv midpoint_explicit.o ~/lib/midpoint_explicit.o
#
echo "Normal end of execution."
