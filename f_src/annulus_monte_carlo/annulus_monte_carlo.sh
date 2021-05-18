#! /bin/bash
#
gfortran -c -Wall annulus_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv annulus_monte_carlo.o ~/lib
#
echo "Normal end of execution."
