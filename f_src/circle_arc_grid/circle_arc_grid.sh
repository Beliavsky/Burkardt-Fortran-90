#! /bin/bash
#
gfortran -c -Wall circle_arc_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv circle_arc_grid.o ~/lib/circle_arc_grid.o
#
echo "Normal end of execution."
