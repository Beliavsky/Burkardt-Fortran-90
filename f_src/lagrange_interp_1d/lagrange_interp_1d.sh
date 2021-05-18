#! /bin/bash
#
gfortran -c -Wall lagrange_interp_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv lagrange_interp_1d.o ~/lib/lagrange_interp_1d.o
#
echo "Normal end of execution."
