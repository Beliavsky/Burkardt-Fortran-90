#! /bin/bash
#
gfortran -c -Wall jacobi_eigenvalue.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv jacobi_eigenvalue.o ~/lib/jacobi_eigenvalue.o
#
echo "Normal end of execution."
