#! /bin/bash
#
gfortran -c -Wall ellipsoid_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ellipsoid_grid.o ~/lib/ellipsoid_grid.o
#
echo "Normal end of execution."
