#! /bin/bash
#
gfortran -c -Wall vandermonde_approx_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv vandermonde_approx_1d.o ~/lib/vandermonde_approx_1d.o
#
echo "Normal end of execution."
