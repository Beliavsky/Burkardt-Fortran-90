#! /bin/bash
#
gfortran -c -Wall triangle01_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle01_monte_carlo.o ~/lib/triangle01_monte_carlo.o
#
echo "Normal end of execution."
