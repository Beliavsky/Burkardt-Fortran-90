#! /bin/bash
#
gfortran -c -Wall polygon_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o polygon_grid_test polygon_grid_test.o $HOME/lib/polygon_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm polygon_grid_test.o
#
./polygon_grid_test > polygon_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm polygon_grid_test
#
echo "Normal end of execution."
