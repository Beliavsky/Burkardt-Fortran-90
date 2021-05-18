#! /bin/bash
#
gfortran -c -Wall sphere_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sphere_grid.o ~/lib/sphere_grid.o
#
echo "Normal end of execution."
