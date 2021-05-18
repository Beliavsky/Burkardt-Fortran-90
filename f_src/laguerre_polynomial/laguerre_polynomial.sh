#! /bin/bash
#
gfortran -c -Wall laguerre_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv laguerre_polynomial.o ~/lib/laguerre_polynomial.o
#
echo "Normal end of execution."
