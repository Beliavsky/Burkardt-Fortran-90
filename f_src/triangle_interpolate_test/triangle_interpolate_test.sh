#! /bin/bash
#
gfortran -c -Wall triangle_interpolate_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o triangle_interpolate_test triangle_interpolate_test.o \
  $HOME/lib/triangle_interpolate.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_interpolate_test.o
#
./triangle_interpolate_test > triangle_interpolate_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle_interpolate_test
#
echo "Normal end of execution."
