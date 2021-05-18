#! /bin/bash
#
gfortran -c -Wall line_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv line_monte_carlo.o ~/lib/line_monte_carlo.o
#
echo "Normal end of execution."
