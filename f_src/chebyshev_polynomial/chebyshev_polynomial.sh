#! /bin/bash
#
gfortran -c -Wall chebyshev_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv chebyshev_polynomial.o ~/lib/chebyshev_polynomial.o
#
echo "Normal end of execution."
