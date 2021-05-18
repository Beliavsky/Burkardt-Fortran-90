#! /bin/bash
#
gfortran -c -Wall cube_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cube_monte_carlo.o ~/lib/cube_monte_carlo.o
#
echo "Normal end of execution."
