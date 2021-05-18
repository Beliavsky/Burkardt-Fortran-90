#! /bin/bash
#
gfortran -c -Wall wedge_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv wedge_grid.o ~/lib/wedge_grid.o
#
echo "Normal end of execution."
