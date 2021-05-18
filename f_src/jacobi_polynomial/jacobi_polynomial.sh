#! /bin/bash
#
gfortran -c -Wall jacobi_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv jacobi_polynomial.o ~/lib/jacobi_polynomial.o
#
echo "Normal end of execution."
