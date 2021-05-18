#! /bin/bash
#
gfortran -c -Wall cycle_brent.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cycle_brent.o ~/lib/cycle_brent.o
#
echo "Normal end of execution."
