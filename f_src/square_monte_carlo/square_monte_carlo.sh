#! /bin/bash
#
gfortran -c -Wall square_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv square_monte_carlo.o ~/lib/square_monte_carlo.o
#
echo "Normal end of execution."
