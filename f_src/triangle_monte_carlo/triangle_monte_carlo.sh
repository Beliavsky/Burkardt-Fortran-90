#! /bin/bash
#
gfortran -c -Wall triangle_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_monte_carlo.o ~/lib/triangle_monte_carlo.o
#
echo "Normal end of execution."
