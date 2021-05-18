#! /bin/bash
#
gfortran -c -Wall disk_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv disk_monte_carlo.o ~/lib/disk_monte_carlo.o
#
echo "Normal end of execution."
