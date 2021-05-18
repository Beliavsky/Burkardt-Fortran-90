#! /bin/bash
#
gfortran -c -Wall sparse_grid_hw.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sparse_grid_hw.o ~/lib/sparse_grid_hw.o
#
echo "Normal end of execution."
