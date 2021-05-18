#! /bin/bash
#
gfortran -c -Wall triangle_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle_grid_test.o $HOME/lib/triangle_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_grid_test.o
#
mv a.out triangle_grid_test
./triangle_grid_test > triangle_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle_grid_test
#
echo "Normal end of execution."
