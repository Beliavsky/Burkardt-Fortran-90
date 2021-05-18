#! /bin/bash
#
gfortran -c -Wall toeplitz_cholesky.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv toeplitz_cholesky.o ~/lib/toeplitz_cholesky.o
#
echo "Normal end of execution."
