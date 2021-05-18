#! /bin/bash
#
gfortran -c -Wall vandermonde.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv vandermonde.o ~/lib/vandermonde.o
#
echo "Normal end of execution."
