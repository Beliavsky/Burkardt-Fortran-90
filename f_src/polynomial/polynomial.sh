#! /bin/bash
#
gfortran -c -Wall polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv polynomial.o ~/lib/polynomial.o
#
echo "Normal end of execution."
