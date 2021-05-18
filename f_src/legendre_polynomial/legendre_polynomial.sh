#! /bin/bash
#
gfortran -c -Wall legendre_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv legendre_polynomial.o ~/lib/legendre_polynomial.o
#
echo "Normal end of execution."
