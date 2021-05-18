#! /bin/bash
#
gfortran -c -Wall lagrange_nd.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv lagrange_nd.o ~/lib/lagrange_nd.o
#
echo "Normal end of execution."
