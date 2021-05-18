#! /bin/bash
#
gfortran -c -Wall sphere_llq_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sphere_llq_grid.o ~/lib/sphere_llq_grid.o
#
echo "Normal end of execution."
