#! /bin/bash
#
gfortran -c -Wall triangle_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_grid.o ~/lib/triangle_grid.o
#
echo "Normal end of execution."
