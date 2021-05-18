#! /bin/bash
#
gfortran -c -Wall circle_arc_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o circle_arc_grid_test circle_arc_grid_test.o $HOME/lib/circle_arc_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm circle_arc_grid_test.o
#
./circle_arc_grid_test > circle_arc_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm circle_arc_grid_test
#
echo "Normal end of execution."
