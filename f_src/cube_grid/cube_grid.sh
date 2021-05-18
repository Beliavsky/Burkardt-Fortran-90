#! /bin/bash
#
gfortran -c -Wall cube_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cube_grid.o ~/lib/cube_grid.o
#
echo "Normal end of execution."
