#! /bin/bash
#
gfortran -c -Wall toeplitz.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv toeplitz.o ~/lib/toeplitz.o
#
echo "Normal end of execution."
