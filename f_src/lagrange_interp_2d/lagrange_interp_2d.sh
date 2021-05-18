#! /bin/bash
#
gfortran -c -Wall lagrange_interp_2d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv lagrange_interp_2d.o ~/lib/lagrange_interp_2d.o
#
echo "Normal end of execution."
