#! /bin/bash
#
gfortran -c -Wall hypercube_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hypercube_monte_carlo.o ~/lib/hypercube_monte_carlo.o
#
echo "Normal end of execution."
