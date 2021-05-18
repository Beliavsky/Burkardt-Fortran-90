#! /bin/bash
#
gfortran -c -Wall fem_basis.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem_basis.o ~/lib/fem_basis.o
#
echo "Normal end of execution."
