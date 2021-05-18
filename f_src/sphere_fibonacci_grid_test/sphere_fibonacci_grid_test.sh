#! /bin/bash
#
gfortran -c -Wall sphere_fibonacci_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o sphere_fibonacci_grid_test sphere_fibonacci_grid_test.o \
  $HOME/lib/sphere_fibonacci_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_fibonacci_grid_test.o
#
./sphere_fibonacci_grid_test > sphere_fibonacci_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sphere_fibonacci_grid_test
#
gnuplot < sphere_fibonacci_grid_n1000_commands.txt
#
echo "Normal end of execution."
