#! /bin/bash
#
gfortran -c -Wall ellipsoid_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ellipsoid_grid_test.o $HOME/lib/ellipsoid_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ellipsoid_grid_test.o
#
mv a.out ellipsoid_grid_test
./ellipsoid_grid_test > ellipsoid_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ellipsoid_grid_test
#
echo "Normal end of execution."
