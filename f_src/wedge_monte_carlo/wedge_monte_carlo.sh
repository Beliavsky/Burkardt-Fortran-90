#! /bin/bash
#
gfortran -c -Wall wedge_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv wedge_monte_carlo.o ~/lib/wedge_monte_carlo.o
#
echo "Normal end of execution."
