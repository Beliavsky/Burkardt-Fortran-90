#! /bin/bash
#
gfortran -c -Wall pyramid_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pyramid_monte_carlo.o ~/lib/pyramid_monte_carlo.o
#
echo "Normal end of execution."
