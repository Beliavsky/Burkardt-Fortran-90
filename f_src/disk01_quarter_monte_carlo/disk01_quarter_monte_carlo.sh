#! /bin/bash
#
gfortran -c -Wall disk01_quarter_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv disk01_quarter_monte_carlo.o ~/lib/disk01_quarter_monte_carlo.o
#
echo "Normal end of execution."
