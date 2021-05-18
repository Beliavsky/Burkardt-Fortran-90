#! /bin/bash
#
gfortran -c -Wall spline.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv spline.o ~/lib/spline.o
#
echo "Normal end of execution."
