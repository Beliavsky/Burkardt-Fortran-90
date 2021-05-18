#! /bin/bash
#
gfortran -c -Wall ball_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ball_grid.o ~/lib/ball_grid.o
#
echo "Normal end of execution."
