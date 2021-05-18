#! /bin/bash
#
gfortran -c -Wall hypercube_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o hypercube_grid_test hypercube_grid_test.o $HOME/lib/hypercube_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hypercube_grid_test.o
#
./hypercube_grid_test > hypercube_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hypercube_grid_test
#
echo "Normal end of execution."
