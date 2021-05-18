#! /bin/bash
#
gfortran -c -Wall quadrature_least_squares.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv quadrature_least_squares.o ~/lib/quadrature_least_squares.o
#
echo "Normal end of execution."
