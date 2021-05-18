#! /bin/bash
#
gfortran -c -Wall circle_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv circle_monte_carlo.o ~/lib/circle_monte_carlo.o
#
echo "Normal end of execution."
