#! /bin/bash
#
gfortran -c -Wall row_echelon_integer.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv row_echelon_integer.o ~/lib/row_echelon_integer.o
#
echo "Normal end of execution."
