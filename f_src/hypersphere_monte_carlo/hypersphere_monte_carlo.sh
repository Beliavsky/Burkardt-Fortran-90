#! /bin/bash
#
gfortran -c -Wall hypersphere_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hypersphere_monte_carlo.o ~/lib/hypersphere_monte_carlo.o
#
echo "Normal end of execution."
