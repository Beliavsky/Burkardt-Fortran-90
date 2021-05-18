#! /bin/bash
#
gfortran -c -Wall polynomials.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv polynomials.o ~/lib/polynomials.o
#
echo "Normal end of execution."
