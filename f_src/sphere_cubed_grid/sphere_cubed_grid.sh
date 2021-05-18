#! /bin/bash
#
gfortran -c -Wall sphere_cubed_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sphere_cubed_grid.o ~/lib/sphere_cubed_grid.o
#
echo "Normal end of execution."
