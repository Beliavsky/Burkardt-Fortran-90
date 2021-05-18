#! /bin/bash
#
gfortran -c -Wall sphere_llq_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o sphere_llq_grid_test sphere_llq_grid_test.o $HOME/lib/sphere_llq_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_llq_grid_test.o
#
./sphere_llq_grid_test > sphere_llq_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sphere_llq_grid_test
#
gnuplot < sphere_llq_grid_commands.txt
#
echo "Normal end of execution."
