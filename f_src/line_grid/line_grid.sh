#! /bin/bash
#
gfortran -c -Wall line_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv line_grid.o ~/lib/line_grid.o
#
echo "Normal end of execution."
