#! /bin/bash
#
gfortran -c -Wall ellipse_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ellipse_monte_carlo.o ~/lib/ellipse_monte_carlo.o
#
echo "Normal end of execution."
