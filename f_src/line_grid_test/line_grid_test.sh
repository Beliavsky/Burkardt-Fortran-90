#! /bin/bash
#
gfortran -c -Wall line_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o line_grid_test line_grid_test.o $HOME/lib/line_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm line_grid_test.o
#
./line_grid_test > line_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm line_grid_test
#
echo "Normal end of execution."
