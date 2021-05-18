#! /bin/bash
#
gfortran -c -Wall ellipse_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ellipse_grid_test.o $HOME/lib/ellipse_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ellipse_grid_test.o
#
mv a.out ellipse_grid_test
./ellipse_grid_test > ellipse_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ellipse_grid_test
#
echo "Normal end of execution."
