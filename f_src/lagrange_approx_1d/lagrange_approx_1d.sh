#! /bin/bash
#
gfortran -c -Wall lagrange_approx_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv lagrange_approx_1d.o ~/lib/lagrange_approx_1d.o
#
echo "Normal end of execution."
