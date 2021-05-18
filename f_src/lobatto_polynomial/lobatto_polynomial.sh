#! /bin/bash
#
gfortran -c -Wall lobatto_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv lobatto_polynomial.o ~/lib/lobatto_polynomial.o
#
echo "Normal end of execution."
