#! /bin/bash
#
gfortran -c -Wall sphere_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sphere_grid_test.o $HOME/lib/sphere_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_grid_test.o
#
mv a.out sphere_grid_test
./sphere_grid_test > sphere_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sphere_grid_test
#
echo "Normal end of execution."
