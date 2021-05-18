#! /bin/bash
#
gfortran -c -Wall clenshaw_curtis_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o clenshaw_curtis_grid_test clenshaw_curtis_grid_test.o $HOME/lib/clenshaw_curtis_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm clenshaw_curtis_grid_test.o
#
./clenshaw_curtis_grid_test > clenshaw_curtis_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm clenshaw_curtis_grid_test
#
echo "Normal end of execution."
