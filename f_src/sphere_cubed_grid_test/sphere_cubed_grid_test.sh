#! /bin/bash
#
gfortran -c -Wall sphere_cubed_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o sphere_cubed_grid_test sphere_cubed_grid_test.o \
  $HOME/lib/sphere_cubed_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_cubed_grid_test.o
#
./sphere_cubed_grid_test > sphere_cubed_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sphere_cubed_grid_test
#
gnuplot < sphere_cubed_grid_lines_commands.txt
gnuplot < sphere_cubed_grid_points_commands.txt
#
echo "Normal end of execution."
