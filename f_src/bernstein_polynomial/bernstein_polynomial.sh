#! /bin/bash
#
gfortran -c -Wall bernstein_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bernstein_polynomial.o ~/lib/bernstein_polynomial.o
#
echo "Normal end of execution."
