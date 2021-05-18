#! /bin/bash
#
gfortran -c -Wall legendre_shifted_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv legendre_shifted_polynomial.o ~/lib/legendre_shifted_polynomial.o
#
echo "Normal end of execution."
