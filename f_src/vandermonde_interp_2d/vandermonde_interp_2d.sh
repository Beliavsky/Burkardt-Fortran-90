#! /bin/bash
#
gfortran -c -Wall vandermonde_interp_2d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv vandermonde_interp_2d.o ~/lib/vandermonde_interp_2d.o
#
echo "Normal end of execution."
