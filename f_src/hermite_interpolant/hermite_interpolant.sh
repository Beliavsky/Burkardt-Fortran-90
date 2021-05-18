#! /bin/bash
#
gfortran -c -Wall hermite_interpolant.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hermite_interpolant.o ~/lib/hermite_interpolant.o
#
echo "Normal end of execution."
