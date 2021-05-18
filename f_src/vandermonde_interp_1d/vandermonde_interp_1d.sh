#! /bin/bash
#
gfortran -c -Wall vandermonde_interp_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv vandermonde_interp_1d.o ~/lib/vandermonde_interp_1d.o
#
echo "Normal end of execution."
