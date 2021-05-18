#! /bin/bash
#
gfortran -c -Wall hyperball_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hyperball_monte_carlo.o ~/lib/hyperball_monte_carlo.o
#
echo "Normal end of execution."
