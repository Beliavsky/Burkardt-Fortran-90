#! /bin/bash
#
gfortran -c -Wall hermite_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hermite_polynomial.o ~/lib/hermite_polynomial.o
#
echo "Normal end of execution."
