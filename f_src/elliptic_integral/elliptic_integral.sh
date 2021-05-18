#! /bin/bash
#
gfortran -c -Wall elliptic_integral.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv elliptic_integral.o ~/lib/elliptic_integral.o
#
echo "Normal end of execution."
