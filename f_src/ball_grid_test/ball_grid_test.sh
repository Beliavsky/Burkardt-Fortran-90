#! /bin/bash
#
gfortran -c -Wall ball_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o ball_grid_test ball_grid_test.o $HOME/lib/ball_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ball_grid_test.o
#
./ball_grid_test > ball_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ball_grid_test
#
echo "Normal end of execution."
