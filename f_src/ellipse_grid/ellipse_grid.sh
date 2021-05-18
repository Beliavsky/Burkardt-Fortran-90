#! /bin/bash
#
gfortran -c -Wall ellipse_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ellipse_grid.o ~/lib/ellipse_grid.o
#
echo "Normal end of execution."
