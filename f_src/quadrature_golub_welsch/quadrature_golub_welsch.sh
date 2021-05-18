#! /bin/bash
#
gfortran -c -Wall quadrature_golub_welsch.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv quadrature_golub_welsch.o ~/lib/quadrature_golub_welsch.o
#
echo "Normal end of execution."
