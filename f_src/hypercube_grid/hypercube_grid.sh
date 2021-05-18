#! /bin/bash
#
gfortran -c -Wall hypercube_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hypercube_grid.o ~/lib/hypercube_grid.o
#
echo "Normal end of execution."
