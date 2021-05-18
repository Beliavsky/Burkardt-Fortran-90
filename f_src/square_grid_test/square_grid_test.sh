#! /bin/bash
#
gfortran -c -Wall square_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o square_grid_test square_grid_test.o $HOME/lib/square_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm square_grid_test.o
#
./square_grid_test > square_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm square_grid_test
#
echo "Normal end of execution."
