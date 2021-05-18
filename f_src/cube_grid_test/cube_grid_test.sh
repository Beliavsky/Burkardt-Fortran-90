#! /bin/bash
#
gfortran -c -Wall cube_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cube_grid_test cube_grid_test.o $HOME/lib/cube_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cube_grid_test.o
#
./cube_grid_test > cube_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cube_grid_test
#
echo "Normal end of execution."
