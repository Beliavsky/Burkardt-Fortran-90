#! /bin/bash
#
gfortran -c -Wall fem1d_lagrange.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem1d_lagrange.o ~/lib/fem1d_lagrange.o
#
echo "Normal end of execution."
