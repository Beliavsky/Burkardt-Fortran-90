#! /bin/bash
#
gfortran -c -Wall jacobi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv jacobi.o ~/lib/jacobi.o
#
echo "Normal end of execution."
