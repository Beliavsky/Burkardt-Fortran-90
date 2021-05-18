#! /bin/bash
#
gfortran -c -Wall simplex_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o simplex_grid_test simplex_grid_test.o $HOME/lib/simplex_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm simplex_grid_test.o
#
./simplex_grid_test > simplex_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm simplex_grid_test
#
echo "Normal end of execution."
