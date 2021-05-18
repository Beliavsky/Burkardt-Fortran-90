#! /bin/bash
#
gfortran -c -Wall sphere_llt_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sphere_llt_grid.o ~/lib/sphere_llt_grid.o
#
echo "Normal end of execution."
