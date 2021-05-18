#! /bin/bash
#
gfortran -c -Wall pyramid_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pyramid_grid_test.o $HOME/lib/pyramid_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pyramid_grid_test.o
#
mv a.out pyramid_grid_test
./pyramid_grid_test > pyramid_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pyramid_grid_test
#
gnuplot < pyramid_unit_commands.txt
#
echo "Normal end of execution."
