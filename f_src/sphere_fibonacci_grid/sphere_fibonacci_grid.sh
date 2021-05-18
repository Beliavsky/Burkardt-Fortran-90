#! /bin/bash
#
gfortran -c -Wall sphere_fibonacci_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sphere_fibonacci_grid.o ~/lib/sphere_fibonacci_grid.o
#
echo "Normal end of execution."
