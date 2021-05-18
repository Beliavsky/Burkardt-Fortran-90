#! /bin/bash
#
gfortran -c -Wall ellipsoid_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ellipsoid_monte_carlo.o ~/lib/ellipsoid_monte_carlo.o
#
echo "Normal end of execution."
